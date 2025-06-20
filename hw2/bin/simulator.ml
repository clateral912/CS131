(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_bot_32 = 0x400000          (* lowest valid address *)
let mem_top_32 = 0x410000          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)
let dummy = Int64.max_int

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
(* IMPORTANT 一个sbyte就是一个字节！*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 8th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
(* 将一个int64类型变量拆成8个字节，按照小端序（低位在前）存在列表中*)
let sbytes_of_int64 (i:int64) : sbyte list =
  (* Char和Int64是库*)
  let open Char in 
  let open Int64 in
  (*将n逻辑右移i位，logand的意思是64位长整数按位与，0xff为掩码，意思是只取低8bit， 
  to_int是将长整数转化为32位OCaml标准整数，
  最后chr表示取int对应的char*)
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = 
    match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
(* when用于处理条件分支的特殊情况， 可以理解为带if的模式匹配 *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    (* 将s的第i位放到acc列表前面，从后往前 *)
    | i -> loop ((Byte s.[i])::acc) (pred i)  (* pred即为i的前序，succ为i的后继 *)
  in
  (* @@操作符表示右结合的嵌套函数调用 ，而且优先级非常低，低于任何运算，因此
  下面的表达式优先计算length - 1*)
  (* \x为十六进制转义序列开头，后接两位数字（比如下面的00）表示一个十六进制的char字面量*)
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | _ -> ()
  in
  (* 分号表示执行所有分号分隔的表达式，并只保留最后一个表达式的求值结果 *)
  (* iter的意思是对列表中的每一个元素执行一次操作（函数f），并且不关心返回值
    List.iter接受两个参数，第一个为操作函数f，第二个是要处理的列表list*)
  List.iter check args;
  (* 最后的表达式默认为该函数的返回值，即一个sbyte列表 *)
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag;
   InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

     [if !debug_simulator then print_endline @@ string_of_ins u; ...]

*)
let debug_simulator = ref false


(* override some useful operators  *)
let ( +. ) = Int64.add
let ( -. ) = Int64.sub
let ( *. ) = Int64.mul
let ( <. ) a b = (Int64.compare a b) < 0
let ( >. ) a b = (Int64.compare a b) > 0
let ( <=. ) a b = (Int64.compare a b) <= 0
let ( >=. ) a b = (Int64.compare a b) >= 0

(* Interpret a condition code with respect to the given flags. *)
(* !!! Check the Specification for Help *)
(* 注意！ 判断的是SRC2 ? SRC1，因此若subq的结果为正，那么应该返回Gt*)
let rec interp_cnd {fo; fs; fz} : cnd -> bool = fun x -> 
  match x with
  | Eq -> fz
  | Neq -> not fz
  (*注意！整数运算会有溢出问题，若不溢出且fs为0,就是小于的情况
    若溢出了，符号位就反转了，有以下情况：
    正数减去大负数，向上溢出成负数，此时fs = fo = 1
    负数减去大正数，向下溢出成正数，此时fs = 0，f0 =0
    不溢出，SRC2 - SRC1为整数，即Gt情况，fs = f0 = 0 
    因此可以归纳出，(fs = fz) and (not fz)就是我们要的Gt情况*)
  | Gt -> (fs = fo) && (not fz)
  | Ge -> fs = fo
  | Lt -> fs <> fo
  | Le -> not ((fs = fo) && (not fz))


(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  match addr with
  | n when n <= mem_top && n >= mem_bot -> Some (Int64.to_int(n -. mem_bot))
  | _ -> None

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* Raise X86lite_segfault when addr is invalid. *)
let map_addr_segfault (addr:quad) : int =
  let checked_addr = (map_addr addr) in
  match checked_addr with
  | None -> raise X86lite_segfault
  | Some i -> i

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags

  We provide the basic structure of step function and helper functions.
  Implement the subroutine below to complete the step function.
  See step function to understand each subroutine and how they 
  are glued together.
*)

let fetch_8byte (m: mach) (base_addr: quad) : sbyte list =
  let base_addr = Int64.to_int (base_addr -. mem_bot)in 
  let fetch_byte (addr: int) : sbyte = m.mem.(addr) in
  let rec fetch_nbyte_from_back_core (m: mach) (n: int) (base_addr: int) : sbyte list = 
    if base_addr > mem_size || base_addr < 0
    then 
      failwith (Printf.sprintf "Memory access out of bounds")
    else
      match n with
      | 0 -> []
      | _ -> fetch_byte(base_addr + n - 1) :: fetch_nbyte_from_back_core m (n - 1) (base_addr)
  in
    List.rev @@ fetch_nbyte_from_back_core m 8 base_addr


(* 为什么必须使用quad类型作为addr的类型？因为这个模拟器试图完全模拟64位系统的行为！
  64位的系统上地址也是64位的，必须提供统一的接口，*)
let readquad (m:mach) (addr:quad) : quad =
  let byte_list = fetch_8byte m addr in
  int64_of_sbytes byte_list


let writequad (m:mach) (addr:quad) (w:quad) : unit =
  let byte_list = sbytes_of_int64 w in 
  let rec write_8byte_from_front (list: sbyte list)(base_addr: int)(offset: int) : unit =
    if base_addr >= mem_size || base_addr <= 0
    then 
      failwith (Printf.sprintf "Memory access out of bounds")
    else
      match offset with
      | 8 -> ()
      | _ -> 
        match list with
        | [] -> failwith("Provided list is shorter than 8!")
        | h :: tl -> 
          m.mem.(base_addr + offset) <- h; 
          write_8byte_from_front tl base_addr (succ(offset))
  in
  let base_addr_int = Int64.to_int (addr -. mem_bot) in
  write_8byte_from_front byte_list base_addr_int 0


let fetchins (m:mach) (addr:quad) : ins = 
  let instruction = fetch_8byte m addr in
  match instruction with
  | [] -> failwith "empty instruction!"
  (* 以下这个模式匹配需要注意！*)
  | InsB0(ins) :: _ -> ins
  | (Byte _ | InsFrag) :: _ -> failwith "Do not contain an instruction"

(* 解析到值 *)
let resolve_value (m: mach) (operand: operand) : int64 = 
  match operand with
  | Imm (Lit x) -> x
  | Reg name -> m.regs.(rind name)
  | Ind1 (Lit offset) -> readquad m @@ (m.regs.(rind Rip) +. offset) (* TODO: 严重错误！没有考虑string*)
  | Ind2 name -> readquad m @@ m.regs.(rind name)
  | Ind3 (Lit offset, name) -> readquad m @@ (m.regs.(rind name) +. offset)
  | _ -> failwith "resolve_value: Resolve SRC error!"

(* 解析到地址 *)
let resolve_addr (m: mach) (operand: operand) : int64 = 
  match operand with
  | Ind1 (Lit offset) -> (m.regs.(rind Rip) +. offset) (* TODO: 严重错误！没有考虑string*)
  | Ind2 name -> m.regs.(rind name)
  | Ind3 (Lit offset, name) -> (m.regs.(rind name) +. offset)
  | _ -> failwith ("resolve_addr: Resolve ADDR error!" ^ (string_of_operand operand))


(* Compute the instruction result.
 * NOTE: See int64_overflow.ml for the definition of the return type
*  Int64_overflow.t. *)
let interp_opcode (m: mach) (o:opcode) (args:int64 list) : Int64_overflow.t = 
    let open Int64 in
    let open Int64_overflow in
    match o, args with
      | Negq, [dest] -> neg (dest)
      | Addq, [src; dest] -> add (src) (dest)
      | Subq, [src; dest] -> sub (dest) (src)
      | Imulq, [src; reg] -> mul (src) (reg)
      | Incq, [dest] -> succ (dest)
      | Decq, [dest] -> pred (dest)
      | Notq, [dest] -> ok(Int64.lognot (dest))
      | Andq, [src; dest] -> ok(Int64.logand (src) (dest))
      | Orq, [src; dest] -> ok(Int64.logor (src) (dest))
      | Xorq, [src; dest] -> ok(Int64.logxor (src) (dest))
      | Sarq, [amt; dest] -> ok(Int64.shift_right (dest) (Int64.to_int amt))
      | Shlq, [amt; dest] -> ok(Int64.shift_left (dest) (Int64.to_int amt))
      | Shrq, [amt; dest] -> ok(Int64.shift_right_logical (dest) (Int64.to_int amt))
      | Leaq, [ind; dest] -> ok(ind)
      | Movq, [src; dest] -> ok(src)
      | Cmpq, [src1; src2] -> sub (src2) (src1)
      | Set _, [dest] -> ok(dest) (* TODO：set需要手动计算！*)
      | Jmp, [src] -> ok(src)
      | J _, [src] -> ok(src)
      | _ -> failwith "interp_opcode: Unknown opcode or illegal parameter"

(** Update machine state with instruction results. *)
let ins_writeback (m: mach) : ins -> int64 -> unit  = 
  let get_dest (operand_list: operand list) : operand = 
    match operand_list with
    | [dest] -> dest
    | [src; dest] -> dest
    | _ -> failwith "get_dest: cannot find dest to write!"
  in 
  let write_dest (res: int64) (operand: operand)  : unit =(
    match operand with
    | Reg name -> (m.regs.(rind name) <- res)
    | ((Ind1 _) | (Ind2 _)| (Ind3 _)) -> writequad m (resolve_addr m operand) res
    | _ -> failwith "write_dest: fail to write dest!")
  in
  let 
    update (operand_list: operand list) (res: int64) = write_dest res (get_dest operand_list) 
  in
  let  
    check_cnd (cnd: cnd) : bool = interp_cnd (m.flags) (cnd)
  in
  fun (instr: ins) (res: int64) : unit ->
    let (opcode, operand_list) = instr in
    let dest_addr = get_dest operand_list in
    match opcode with
    | Cmpq -> () (* cmpq不涉及任何写入 *)
    | J cnd -> 
      if check_cnd(cnd) 
      then
         write_dest res (Reg Rip)
      else ()
    | Set cnd ->(
      match operand_list with
      | [dest] -> 
        let original_val =
          match dest with
          | Reg r -> m.regs.(rind r)
          | _ -> readquad m (resolve_addr m dest)
        in
        let cleared_val = Int64.logand original_val (Int64.lognot 0xffL) in
        if check_cnd(cnd) 
        then
          write_dest (cleared_val +. 1L) dest
        else 
          write_dest (cleared_val) dest
      | _ -> failwith "ins_writeback: Set instruction needs one operand" )
    | _ -> update operand_list res
      



(* mem addr ---> mem array index *)
(* 将所有operand全部解析到值，除了leaq*)
let interp_operands (m:mach) (instr: ins) : int64 list = 
  let (opcode, operand_list) = instr in 
  match (opcode, operand_list) with
  | (Retq, _) -> []
  | (Cmpq, _) -> (
    match operand_list with
    | [src1; src2] -> [resolve_value m src1; resolve_value m src2]
    | _ -> failwith "interp_operands: Interpret cmpq error!")
  | (Leaq, _) ->(
    match operand_list with
    | [ind; dest] -> [resolve_addr m ind; resolve_value m dest]
    | _ -> failwith "interp_operands: Interpret leaq error!")
  | ((Pushq, _) | (Jmp, _) | (J _, _) | (Callq, _)) -> (
    match operand_list with
    | [src] -> [resolve_value m src]
    | _ -> failwith "interp_operands: Interpret single SRC error!")
  | ((Negq, _) | (Incq, _) | (Decq, _) | (Notq, _) | (Popq, _) | (Set _, _)) ->(
    match operand_list with
    | [dest] -> [resolve_value m dest]
    | _ -> failwith "interp_operands: Interpret single DEST error!")
  | ((Addq, _) | (Subq, _) | (Imulq, _) | (Andq, _) | (Orq, _) | (Xorq, _) | 
     (Sarq, _) | (Shlq, _) | (Shrq, _) | (Movq, _)) -> (
    match operand_list with
    | [src; dest] -> [resolve_value m src; resolve_value m dest]
    | _ -> failwith "interp_operands: Interpret single SRC error!")
  
  
type operandType = DEST | SRC | AMT | IND | REG | IMM 

let operand_type_check (operand: operand) (desiredType: operandType) : unit =
  match operand with 
  | Imm _ -> (
    match desiredType with
    | (SRC | AMT | IMM) -> ()
    | _ -> failwith "operand_type_check: Imm Incorrect operand type! ")
  | Reg Rcx ->(
    match desiredType with
    | (SRC | DEST | REG | AMT) -> ()
    | _ -> failwith "operand_type_check: Reg Rcx Incorrect operand type!")
  | Reg _ ->(
    match desiredType with
    | (SRC | DEST | REG) -> ()
    | _ -> failwith "operand_type_check: Reg _ Incorrect operand type!")
  | (Ind1 _ | Ind2 _ | Ind3 _) ->(
    match desiredType with
    | (SRC | DEST | IND) -> ()
    | _ -> failwith "operand_type_check: Ind Incorrect operand type!")
  

let rec operand_list_type_check (actual: operand list) (expect: operandType list) : unit =
  (* 惊为天人的写法！将两个列表同时进行模式匹配！*)
  match (actual, expect) with
  | ([], []) -> ()
  | ((_, []) | ([], _)) -> failwith "operand_list_type_check: Illegal operand num!"
  | (actual_operand :: atl, expect_type :: etl) -> (
    operand_type_check actual_operand expect_type;
    operand_list_type_check (atl) (etl))

let validate_operands (instruction: ins) : unit = 
  let (opcode, operand_list) = instruction in
  let check = operand_list_type_check operand_list in
  match opcode with
  | Retq -> check []
  | (Negq | Incq | Decq | Notq | Popq | Set _) -> check [DEST]
  | (Pushq | Jmp | Callq | J _) -> check [SRC]
  | (Sarq | Shlq | Shrq ) -> check [AMT; DEST]
  | (Addq | Subq | Andq | Orq | Xorq | Movq) -> check [SRC; DEST]
  | Leaq -> check [IND; DEST]
  | Cmpq -> check [SRC; DEST] (* SRC2不能是立即数，非立即数的SRC就是DEST类型*)
  | Imulq -> check [SRC; REG]



let rec crack : ins -> ins list = function
  (* 惊为天人的写法：同时匹配两个参数！*)
  (* 不能将Cmpq crack为Subq，因为Cmpq不修改寄存器或内存*)
  | (Retq, []) ->
    crack ((Popq, [Reg Rip]))
  | (Pushq, [src]) -> 
    [(Leaq, [Ind3 (Lit (-8L), Rsp); Reg Rsp]); (Movq, [src ;Ind2 Rsp])]
  | (Popq, [dest]) ->
    [(Movq, [Ind2 Rsp; dest]); (Leaq, [Ind3 (Lit (8L), Rsp); Reg Rsp]);] 
  | (Callq, [src]) ->
    crack ((Pushq, [Reg Rip])) @ [(Movq, [src; Reg Rip])]
  | (Jmp, [src]) -> 
    [(Movq, [src; Reg Rip])]
  | ((Callq, _) | (Pushq, _) | (Popq, _)) -> failwith "crack: Illegal crack ins operand type!"
  | _ as instr -> [instr]
 
(* TODO: double check against spec *)
let set_flags (m:mach) (op:opcode) (ws: quad list) (w : Int64_overflow.t) : unit =
  let setSF () = m.flags.fs <- true in
  let setZF () = m.flags.fz <- true in
  let setOF () = m.flags.fo <- true in

  let resetSF () = m.flags.fs <- false in
  let resetZF ()= m.flags.fz <- false in
  let resetOF ()= m.flags.fo <- false in

  let 
    update_sf_zf (res: Int64_overflow.t) : unit =
      let res = res.value in
      if res < 0L then setSF () else resetSF ();
      if res = 0L then setZF () else resetZF ()
  in
  let update_of (res: Int64_overflow.t) : unit = 
    if res.overflow then setOF () else resetOF()
  in 
  let update_shift (res: Int64_overflow.t) (op: opcode) : unit =
    let amt_val, dest = match ws with 
      | [amt; dest] -> amt, dest
      | _ -> failwith "set_flags: Illegal operand for sarq" 
    in
    if amt_val <> 0L then 
      update_sf_zf w;
    match op with
    | Sarq -> 
      if amt_val = 1L then resetOF () else ()
    | Shlq -> 
      if amt_val = 1L then
        let dest_msb = Int64.shift_right_logical (dest) 62 in
        match dest_msb with
        | (0L | 3L) -> resetOF ()
        | _ -> setOF ()
      else
        ()
    | Shrq ->
      if amt_val <> 0L then(
        if res.value = 0L then setZF () else resetZF ();
        let res_msb = Int64.shift_right_logical (res.value) 63 in
        match res_msb with
          | 1L -> setSF ()
          | _ -> resetSF ();
        if amt_val = 1L then
          let dest_msb = Int64.shift_right_logical (dest) 63 in
          match dest_msb with
          | 1L -> setOF ()
          | _ -> resetOF ())
      else ()
    | _ -> failwith "update_shift: Not a shift opcode!"

  in
  match op with
  | Negq -> 
    update_sf_zf w;
    let dest_val = match ws with 
      | [dest] -> dest 
      | _ -> failwith "set_flags: Illegal operand for negq" 
    in
      if dest_val = Int64.min_int then setOF () else resetOF ()
  | (Sarq | Shlq | Shrq) -> update_shift w op
  | (Andq | Orq | Xorq) -> update_sf_zf w; resetOF ()
  (* 不需要修改标志位的opcode *)
  | (Notq | Set _ | Leaq | Movq | Pushq | Popq | Jmp | Callq | Retq | J _) -> ()
  | _ -> update_sf_zf w; update_of w 





let step (m:mach) : unit =
  (* execute an instruction *)
  let (op, args) as ins = fetchins m m.regs.(rind Rip) in
  validate_operands ins;
  
  (* Some instructions involve running two or more basic instructions. 
   * For other instructions, just return a list of one instruction.
   * See the X86lite specification for details. *)
  let uops: ins list = crack (op,args) in

  m.regs.(rind Rip) <- m.regs.(rind Rip) +. ins_size;

  List.iter
    (fun (uop,_ as u) ->
     if !debug_simulator then print_endline @@ string_of_ins u;
     let ws = interp_operands m u in
     let res = interp_opcode m uop ws in
     ins_writeback m u @@ res.Int64_overflow.value;
     set_flags m op ws res
    ) uops

(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the 
   machine halts. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)
            due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.
     HINT: consider building a mapping from symboli Lbl to memory address

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let is_size (is: ins list): quad = 
  failwith "is_size not implemented"

let ds_size (ds: data list): quad = 
  failwith "ds_size not implemented"

let assemble (p:prog) : exec =
  failwith "assemble unimplemented"

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
   failwith "load not implemented"