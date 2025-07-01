(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86
open Llutil
module Platform = Util.Platform

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understanding this entire file and
   how it fits with the compiler pipeline before making changes.  The suggested
   plan for implementing the compiler is provided on the project web page.
*)

(* helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq -> X86.Eq
  | Ll.Ne -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge
;;

(* 对于参数传递中结构体的处理：caller需要在栈上保存结构体本身，作为参数的永远是指向结构体本身的指针
   callee永远会将结构体作为指针处理*)

(* override some useful operators *)
let ( +. ) = Int64.add
let ( -. ) = Int64.sub
let ( *. ) = Int64.mul
let ( <. ) a b = Int64.compare a b < 0
let ( >. ) a b = Int64.compare a b > 0
let ( <=. ) a b = Int64.compare a b <= 0
let ( >=. ) a b = Int64.compare a b >= 0

(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid.  However,
   since there are an unlimited number of %uids and only 16 registers,
   doing so effectively is quite difficult.  We will see later in the
   course how _register allocation_ algorithms can do a good job at
   this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'.  A stack layout maps a uid to an X86 operand for
   accessing its contents.  For this compilation strategy, the operand
   is always an offset from %rbp (in bytes) that represents a storage slot in
   the stack.
*)

type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt =
  { tdecls : (tid * ty) list
  ; layout : layout
  }

(* useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m

(* Complete this helper function, which computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions. We will test this function as part of
   the hidden test cases.

   You might find it useful for compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
(* 返回caller栈帧中args相对新rbp的位置offset，注意，caller的栈帧在高地址！因此你的地址是rbp + n
   而非rbp - n*)
let arg_loc (n : int) : operand =
  match n with
  | 0 -> Reg Rdi
  | 1 -> Reg Rsi
  | 2 -> Reg Rdx
  | 3 -> Reg Rcx
  | 4 -> Reg R08
  | 5 -> Reg R09
  | x -> Ind3 (Lit (8L +. (Int64.of_int (x - 5) *. 8L)), Rbp)
;;

(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

   NOTE: two important facts about global identifiers:

   (1) You should use (Platform.mangle gid) to obtain a string
   suitable for naming a global label on your platform (OS X expects
   "_main" while linux expects "main").

   (2) 64-bit assembly labels are not allowed as immediate operands.
   That is, the X86 code: movq _gid %rax which looks like it should
   put the address denoted by _gid into %rax is not allowed.
   Instead, you need to compute an %rip-relative address using the
   leaq instruction:   leaq _gid(%rip) %rax.

   One strategy for compiling instruction operands is to use a
   designated register (or registers) for holding the values being
   manipulated by the LLVM IR instruction. You might find it useful to
   implement the following helper function, whose job is to generate
   the X86 instruction that moves an LLVM operand into a designated
   destination (usually a register).
*)
(* 将Null Const和Id的引用解析到值，对于全局变量，我们只是将全局变量的地址存入dest中，而不是搬运全局变量的值 *)
let compile_operand ({ tdecls; layout } : ctxt) (dest : X86.operand) : Ll.operand -> ins
  = function
  | Null -> Movq, [ Imm (Lit 0L); dest ]
  | Const x -> Movq, [ Imm (Lit x); dest ]
  | Gid gid ->
    let gid_label = Platform.mangle gid in
    Leaq, [ Ind3 (Lbl gid_label, Rip); Reg Rax ]
  | Id uid ->
    let arg_operand = lookup layout uid in
    Movq, [ arg_operand; dest ]
;;

(* 在layout中找对应的uid，返回该uid相对于rbp的偏移量*)
let rec layout_loc (layout : layout) (arg : uid) : quad =
  match layout with
  | [] -> failwith "layout_loc: cannot find arg in layout!"
  | (uid, ind) :: tl ->
    if arg = uid
    then (
      match ind with
      | Ind3 (Lit x, Rbp) -> x
      | _ -> failwith "layout_loc: illegal layout operand")
    else layout_loc tl arg
;;

(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: Don't forget to preserve caller-save registers (only if needed). ]

   [ NOTE: Remember, call can use labels as immediates! You shouldn't need to 
     perform any RIP-relative addressing for this one. ]

   [ NOTE: It is the caller's responsibility to clean up arguments pushed onto
     the stack, so you must free the stack space after the call returns. (But 
     see below about alignment.) ]

   [ NOTE: One important detail about the ABI besides the conventions is that, 
  at the time the [callq] instruction is invoked, %rsp *must* be 16-byte aligned.  
  However, because LLVM IR provides the Alloca instruction, which can dynamically
  allocate space on the stack, it is hard to know statically whether %rsp meets
  this alignment requirement.  Moroever: since, according to the calling 
  conventions, stack arguments numbered > 6 are pushed to the stack, we must take
  that into account when enforcing the alignment property.  

  We suggest that, for a first pass, you *ignore* %rsp alignment -- only a few of 
  the test cases rely on this behavior.  Once you have everything else working,
  you can enforce proper stack alignment at the call instructions by doing 
  these steps: 
    1. *before* pushing any arguments of the call to the stack, ensure that the
    %rsp is 16-byte aligned.  You can achieve that with the x86 instruction:
    `andq $-16, %rsp`  (which zeros out the lower 4 bits of %rsp, possibly 
    "allocating" unused padding space on the stack)

    2. if there are an *odd* number of arguments that will be pushed to the stack
    (which would break the 16-byte alignment because stack slots are 8 bytes),
    allocate an extra 8 bytes of padding on the stack. 
    
    3. follow the usual calling conventions - any stack arguments will still leave
    %rsp 16-byte aligned

    4. after the call returns, in addition to freeing up the stack slots used by
    arguments, if there were an odd number of slots, also free the extra padding. 
    
  ]
*)

(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelementptr, you must generate x86 code that performs
   the appropriate arithmetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes.
   (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let rec size_ty (tdecls : (tid * ty) list) (t : Ll.ty) : int =
  match t with
  | Void | I8 | Fun _ -> 0
  | Array (len, t) -> len * size_ty tdecls t
  | Ptr _ | I1 | I64 -> 8
  | Namedt name -> size_ty tdecls (lookup tdecls name)
  | Struct ty_list ->
    let f (acc : int) (t : ty) = acc + size_ty tdecls t in
    List.fold_left f 0 ty_list
;;

(* Generates code that computes a pointer value.

   1. op must be of pointer type: t*

   2. the value of op is the base address of the calculation

   3. the first index in the path is treated as the index into an array
   of elements of type t located at the base address

   4. subsequent indices are interpreted according to the type t:

   - if t is a struct, the index must be a constant n and it
     picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the
       sizes of the types of the previous elements ]

   - if t is an array, the index can be any operand, and its
     value determines the offset within the array.

   - if t is any other type, the path is invalid

   5. if the index is valid, the remainder of the path is computed as
   in (4), but relative to the type f the sub-element picked out
   by the path so far
*)
let compile_gep (ctxt : ctxt) (op : Ll.ty * Ll.operand) (path : Ll.operand list)
  : ins list
  =
  failwith "compile_gep not implemented"
;;

(* prefix the function name [fn] to a label to ensure that the X86 labels are
   globally unique . *)
let mk_lbl (fn : string) (l : string) = fn ^ "." ^ l

(* 设置参数, push多余参数到stack上，Jump到对应的global label*)
let compile_call
  ({ tdecls; layout } : ctxt)
  (call_args : ty * Ll.operand * (ty * Ll.operand) list)
  : X86.ins list
  =
  let _, fn, args = call_args in
  let fn =
    match fn with
    | Gid gid -> gid
    | _ -> failwith "compile_call: illegal function name!"
  in
  let convert_operand ((_, operand) : ty * Ll.operand) : X86.operand =
    match operand with
    | Null -> Imm (Lit 0L)
    | Const x -> Imm (Lit x)
    | Gid gid -> Imm (Lbl (mk_lbl fn gid))
    | Id uid -> lookup layout uid
  in
  (*[arg1, arg2, arg3, arg4, arg5,arg6, arg7,arg8] -->
    [arg1, arg2, arg3, arg4, arg5,arg6]  [arg7,arg8]*)
  let rec seperate
    (cnt : int)
    (reg_args : (ty * Ll.operand) list)
    (stack_args : (ty * Ll.operand) list)
    : (ty * Ll.operand) list * (ty * Ll.operand) list
    =
    if cnt > 6
    then reg_args, stack_args
    else (
      match stack_args with
      | [] -> reg_args, []
      | arg :: tl -> seperate (cnt + 1) (reg_args @ [ arg ]) tl)
  in
  let reg_args, stack_args = seperate 1 [] args in
  let stack_args = List.rev stack_args in
  let setup_reg (idx : int) (arg : ty * Ll.operand) : X86.ins =
    Movq, [ convert_operand arg; arg_loc idx ]
  in
  let setup_stack (arg : ty * Ll.operand) : X86.ins = Pushq, [ convert_operand arg ] in
  let call_ins = [Callq, [Imm(Lbl fn)]] in
  List.mapi setup_reg reg_args @ List.map setup_stack stack_args @ call_ins
;;

(* compiling instructions  -------------------------------------------------- *)

(* The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple of assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few notes:

   - Icmp:  the Setb instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level
*)
let compile_insn (ctxt : ctxt) ((uid : uid), (i : Ll.insn)) : X86.ins list =
  let { tdecls; layout } = ctxt in
  let dest_operand = Ind3 (Lit (layout_loc layout uid), Rbp) in
  match i with
  | Binop (bop, ty, op1, op2) ->
    if ty <> I64
    then failwith "compile_insn: illegal Binop ty"
    else (
      (* TODO: 确认bop的ty有什么用 *)
      let load_operand_ins =
        match bop with
        (* 对于IR， sub op1, op2的顺序是op1 - op2, 而x86 asm的顺序是op2 - op1 *)
        | Sub | Shl | Lshr | Ashr ->
          [ compile_operand ctxt (Reg R11) op1; compile_operand ctxt (Reg Rcx) op2 ]
        | _ -> [ compile_operand ctxt (Reg Rcx) op1; compile_operand ctxt (Reg R11) op2 ]
      in
      let llbop_to_opcode (llbop : bop) : opcode =
        match llbop with
        | Add -> Addq
        | Sub -> Subq
        | Mul -> Imulq
        | Shl -> Shlq
        | Lshr -> Shrq
        | Ashr -> Sarq
        | And -> Andq
        | Or -> Orq
        | Xor -> Xorq
      in
      let rev_ins_list =
        (llbop_to_opcode bop, [ Reg Rcx; Reg R11 ]) :: load_operand_ins
      in
      let rev_ins_list = (Movq, [ Reg R11; dest_operand ]) :: rev_ins_list in
      List.rev rev_ins_list)
  | Icmp (cnd, ty, op1, op2) ->
    let cond = compile_cnd cnd in
    let head =
      [ compile_operand ctxt (Reg R10) op2; compile_operand ctxt (Reg R11) op1 ]
    in
    let ins_list = [ Cmpq, [ Reg R10; Reg R11 ]; Set cond, [ dest_operand ] ] in
    head @ ins_list
  | Alloca ty ->
    let size = Int64.of_int (size_ty tdecls ty) in
    [ Subq, [ Imm (Lit size); Reg Rsp ]; Movq, [ Reg Rsp; dest_operand ] ]
  | Load (ty, ptr) ->
    let load_insn = compile_operand ctxt (Reg Rax) ptr in
    load_insn :: [ Movq, [ Ind2 Rax; Reg Rax ]; Movq, [ Reg Rax; dest_operand ] ]
  | Store (ty, operand, ptr) ->
    let load_insns =
      [ compile_operand ctxt (Reg Rax) ptr; compile_operand ctxt (Reg R10) operand ]
    in
    load_insns @ [ Movq, [ Reg R10; Ind2 Rax ] ]
  | Call (ty, operand, arg_list) -> 
    let prepare_call = compile_call ctxt (ty, operand, arg_list) in
    let get_retval = [(Movq, [Reg Rax; dest_operand])] in
    prepare_call @ get_retval
  | _ -> failwith "compile_insn: Not implemented yet!"
;;

(* compiling terminators  --------------------------------------------------- *)

(* Compile block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional

   [fn] - the name of the function containing this terminator
*)
let compile_terminator (fn : string) (ctxt : ctxt) (t : Ll.terminator) : ins list =
  match t with
  | Br lbl -> [ Jmp, [ Imm (Lbl (mk_lbl fn lbl)) ] ]
  | Cbr (cnd, lbl1, lbl2) ->
    (* 注意！LLVM IR不允许Gid直接作为Cnd比较对象！比较对象必须为i1！无论如何你都要将其加载到Rax中！*)
    let load_cnd_ins = compile_operand ctxt (Reg Rax) cnd in
    [ load_cnd_ins ]
    @ [ Cmpq, [ Imm (Lit 0L); Reg Rax ]
      ; J Neq, [ Imm (Lbl (mk_lbl fn lbl1)) ]
      ; Jmp, [ Imm (Lbl (mk_lbl fn lbl2)) ]
      ]
  | Ret (_, None) -> [ Movq, [ Reg Rbp; Reg Rsp ]; Popq, [ Reg Rbp ]; Retq, [] ]
  (* TODO： 弄清楚ty的作用 *)
  | Ret (_, Some operand) ->
    let tail = [ Movq, [ Reg Rbp; Reg Rsp ]; Popq, [ Reg Rbp ]; Retq, [] ] in
    (match operand with
     | Null -> (Movq, [ Imm (Lit 0L); Reg Rax ]) :: tail
     | Const x -> (Movq, [ Imm (Lit x); Reg Rax ]) :: tail
     (* TODO: 没搞清楚return全局变量的意义何在 *)
     | Gid gid -> (Movq, [ Imm (Lbl (Platform.mangle gid)); Reg Rax ]) :: tail
     | Id uid ->
       let { tdecls = _; layout } = ctxt in
       let value_addr = layout_loc layout uid in
       (Movq, [ Ind3 (Lit value_addr, Rbp); Reg Rax ]) :: tail)
;;

(* compiling blocks --------------------------------------------------------- *)

(* We have left this helper function here for you to complete.
   [fn] - the name of the function containing this block
   [ctxt] - the current context
   [blk]  - LLVM IR code for the block
*)
let compile_block (fn : string) (ctxt : ctxt) (blk : Ll.block) : ins list =
  let { insns; term = _, terminator } = blk in
  let compiled_insns = List.flatten @@ List.map (compile_insn ctxt) insns in
  compiled_insns @ compile_terminator fn ctxt terminator
;;

let compile_lbl_block fn lbl ctxt blk : elem =
  Asm.text (mk_lbl fn lbl) (compile_block fn ctxt blk)
;;

(* compile_fdecl ------------------------------------------------------------ *)

(* 减去Iret *)

(* We suggest that you create a helper function that computes the
   stack layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id
     is also stored as a stack slot.
   - see the discussion about locals
*)
(* Layout结构：lbl_blocks(term_uid :: uids) :: entry_block(term_uid :: uids) :: args *)
(* size的单位是个， 而非byte*)
let stack_layout (args : uid list) (cfg : cfg) : layout * quad =
  let rec args_layout (args : uid list) (size : int64) (layout : layout) : layout * quad =
    match args with
    | [] -> layout, size
    | arg :: tl ->
      args_layout tl (size +. 1L) ((arg, Ind3 (Lit ((size +. 1L) *. -8L), Rbp)) :: layout)
  in
  let rec block_uid ({ insns; term } : block) : uid list =
    let extract (acc : uid list) (insn : uid * insn) : uid list =
      match insn with
      | uid, _ -> uid :: acc
    in
    let term_uid, _ = term in
    term_uid :: List.fold_left extract [] insns
  in
  let rec cfg_uid ((block, lbled_blocks) : cfg) : uid list =
    match lbled_blocks with
    | [] -> block_uid block
    | (_, blk) :: tl -> block_uid blk @ cfg_uid (block, tl)
  in
  let all_uids = cfg_uid cfg @ args in
  args_layout all_uids 0L []
;;

(*TODO: 处理外发参数大小和栈指针对其大小*)
(* 在elem程序段的最后插入分配给定大小的栈的指令 *)
let allocate_stack (size : quad) (code : ins list) : ins list =
  let alloca_ins : ins = Subq, [ Imm (Lit size); Reg Rsp ] in
  code @ [ alloca_ins ]
;;

(* 将regs和栈上的参数搬运到callee的locals中 *)
let move_args_asm (layout : layout) (args : uid list) : ins list =
  let handle_arg (i : int) (arg : uid) : ins list =
    let dest_offset = layout_loc layout arg in
    let src_location = arg_loc i in
    match src_location with
    | Reg _ as reg_loc -> [ Movq, [ reg_loc; Ind3 (Lit dest_offset, Rbp) ] ]
    | _ as mem_loc ->
      [ Movq, [ mem_loc; Reg Rax ]; Movq, [ Reg Rax; Ind3 (Lit dest_offset, Rbp) ] ]
  in
  List.flatten @@ List.mapi handle_arg args
;;

let prologue (name : string) (params : uid list) (layout : layout) (stack_size : quad)
  : ins list
  =
  let adjust_rbp = [ Movq, [ Reg Rsp; Reg Rbp ] ] in
  let save_rbp_ins = (Pushq, [ Reg Rbp ]) :: adjust_rbp in
  let move_args = save_rbp_ins @ move_args_asm layout params in
  allocate_stack stack_size move_args
;;

(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout)

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)

let compile_fdecl
  (tdecls : (tid * ty) list)
  (name : string)
  ({ f_param; f_cfg; _ } as fdecl : fdecl)
  : prog
  =
  let f_layout, f_stack_size = stack_layout f_param f_cfg in
  let ctxt = { tdecls; layout = f_layout } in
  let entry_block, blocks = f_cfg in
  (*加上entry块的所有基本块*)
  let entry_block_insns = compile_block name ctxt entry_block in
  let prologue_insns = prologue name f_param f_layout (f_stack_size *. 8L) in
  let entry_elem : elem =
    { lbl = name; global = true; asm = Text (prologue_insns @ entry_block_insns) }
  in
  let f ((lbl, block) : lbl * block) : elem =
    print_string lbl;
    compile_lbl_block name lbl ctxt block
  in
  entry_elem :: List.map f blocks
;;

(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit : ginit -> X86.data list = function
  | GNull -> [ Quad (Lit 0L) ]
  | GGid gid -> [ Quad (Lbl (Platform.mangle gid)) ]
  | GInt c -> [ Quad (Lit c) ]
  | GString s -> [ Asciz s ]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten
  | GBitcast (_t1, g, _t2) -> compile_ginit g

and compile_gdecl (lbl, g) = compile_ginit g

(* compile_prog ------------------------------------------------------------- *)
let compile_prog { tdecls; gdecls; fdecls; _ } : X86.prog =
  let g (lbl, gdecl) = Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f (name, fdecl) = compile_fdecl tdecls name fdecl in
  List.map g gdecls @ (List.map f fdecls |> List.flatten)
;;
