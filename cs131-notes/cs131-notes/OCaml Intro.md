1. 所有OCaml中的“变量”都是不可变的，或者说，有名字的值都是不可变的
2. 不要在命名中使用dash“-”， 应该使用下划线__underscore
3. 使用（\*\*开头的注释被称为docstring，一般用于let变量绑定之前，静态分析工具会将这个注释和let绑定关联在一起，用于更智能的提示
4. 使用let...in...来定义局部变量，作用域在in后，你可以嵌套使用
 ```ocaml
 let a = 1 in
 let b = 2 in
    a + b;;
- : int = 3
 ```
5. OCaml中=等号可以直接用于if条件中的判等，行为与C/C++中的\==一致，注意，=的negation是<>，而\==在OCaml中表示“物理相等”即两个变量是否位于同一个内存空间，\==的negation是!=
6. 函数也可以绑定到名称上，OCaml中没有返回值的概念，=等号之前是参数，之后是函数体（或者说，返回的东西），REPL无法打印出函数的“值”，而是会显示\<fun>
7. 你可以用~parameterName:param的形式来指定具名参数的值
```ocaml
# let square x = x * x;;
val square : int -> int = <fun>

# square 50;;
- : int = 2500

# String.ends_with;;
- : suffix:string -> string -> bool = <fun>

# String.ends_with ~suffix:"less" "stateless";;
- : bool = true
```
8. 也可以使用fun关键字指定一个匿名函数，调用函数的方法是函数名之后直接空格加参数，不需要括号
```ocaml
# (fun x -> x * x) 50;;
- : int = 2500
```

9. 拥有多个参数的函数会被视为高阶函数，即柯里化curry嵌套后的函数，从REPL给出的函数类型可以看出端倪：
```ocaml
# let cat a b = a ^ " " ^ b;;
val cat : string -> string -> string = <fun>

# let cat_hi = cat "hi";;
val cat_hi : string -> string = <fun>
```
10. 当你只传入一个参数时，你得到的返回值是一个函数，其中的某个参数已经被替换为你刚刚传入的值
11. OCaml支持高级函数，即函数将某个函数作为参数传入，在参数表示上写为圆括号(\<fun>)的形式
12. 函数原生支持多态：函数本身不关心参数的具体类型，这点在以下的List.map中体现出来，其中的任意类型为\`a \`b， 发音为alpha和beta
```ocaml
# List.map;;
- : ('a -> 'b) -> 'a list -> 'b list = <fun>

# List.map (fun x -> x * x);;
- : int list -> int list = <fun>

# List.map (fun x -> x * x) [0; 1; 2; 3; 4; 5];;
- : int list = [0; 1; 4; 9; 16; 25]
```
13. OCaml使用unit类型来标记副作用,unit出现表示了函数在执行过程中出现了除了函数体指定的内容之外的副作用，比如常见的IO操作
```ocaml
# read_line;;
- : unit -> string = <fun>

# read_line ();;
caramba
- : string = "caramba"

# print_endline;;
- : string -> unit = <fun>

# print_endline "¿Cuándo se come aquí?";;
¿Cuándo se come aquí?
- : unit = ()
```
14. 使用递归函数代替for loop控制流，非常妙的想法， 使用rec关键字定义递归函数range，表示生成lo到hi的连续整数列表：
```ocaml
# let rec range lo hi =
    if lo > hi then
      []
    else
      lo :: range (lo + 1) hi;;
val range : int -> int -> int list = <fun>

# range 2 5;;
- : int list = [2; 3; 4; 5]
```
 15. 如果你想要加两个浮点数，必须使用+.而不是+
 16. OCaml中没有隐式类型转换！1 +. 2.6或者1 + 2.6都是错误的表达式！
	 如果你想加一个浮点数和一个整数，请你使用float_of_int 显式转换函数
	 ```ocaml
# float_of_int 1 +. 2.5;;
- : float = 3.5
```
 17.  ocaml中列表的类型为: contentType list, list之前的type为所存储的内容的type
 ```ocaml
# [];;
- : 'a list = []

# [1; 2; 3];;
- : int list = [1; 2; 3]

# [false; false; true];;
- : bool list = [false; false; true]

# [[1; 2]; [3]; [4; 5; 6]];;
- : int list list = [[1; 2]; [3]; [4; 5; 6]]
```
18. 使用模式匹配来定义函数，类似switch case语句，注意x::v表示将u中的第一个元素提取出来，并将剩下的list命名为v
```ocaml
# let rec sum u =
    match u with
    | [] -> 0
    | x :: v -> x + sum v;;
val sum : int list -> int = <fun>

# sum [1; 4; 3; 2; 5];;
- : int = 15

# let rec length u =
    match u with
    | [] -> 0
    | _ :: v -> 1 + length v;; (* _ doesn't define a name; it can't be used in the body *)
val length : 'a list -> int = <fun>

# length [1; 2; 3; 4];;
- : int = 4

# length ["cow"; "sheep"; "cat"];;
- : int = 3

# length [[]];;
- : int = 1

(* 使用了高阶函数的模式匹配 *)
# let square x = x * x;;
val square : int -> int

# let rec map f u =
    match u with
    | [] -> []
    | x :: u -> f x :: map f u;;
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>

# map square [1; 2; 3; 4;];;
- : int list = [1; 4; 9; 16]
```

19. option为某个值的包装，类型为None或Some a'，若什么都没有，则option的值为None，若有值，则option的值为some a'，这种包装强制你在模式匹配中必须处理值为空None的情况，如下是解包一个两层嵌套Option的情况：
```ocaml
# #show option;;
type 'a option = None | Some of 'a

# let f opt = match opt with
    | None -> None
    | Some None -> None
    | Some (Some x) -> Some x;;
val f : 'a option option-> 'a option = <fun>
```
20.  使用单个下划线作为任意通配符，以下是if-else分支与模式匹配的比较：
```ocaml
# let g x =
  if x = "foo" then 1
  else if x = "bar" then 2
  else if x = "baz" then 3
  else if x = "qux" then 4
  else 0;;
val g : string -> int = <fun>

# let g' x = match x with
    | "foo" -> 1
    | "bar" -> 2
    | "baz" -> 3
    | "qux" -> 4
    | _ -> 0;;
val g' : string -> int = <fun>
```
21. OCaml中Tuple是固定长度，但可以存储任意类型变量的数据结构，注意Tuple中各个数据类型之间用\*号隔开：
```ocaml
# (1, "one", 'K');;
- : int * string * char = (1, "one", 'K')

# ([], false);;
- : 'a list * bool = ([], false)
```
22. Variant变体类型，是更高级的enum，实现了类型和值的绑定，还支持异构数据类型（比如特定tuple，或者特定type的list）的定义，在模式匹配时就能直接解包得到数据![[Pasted image 20250617212246.png]]
```ocaml
# type primary_colour = Red | Green | Blue;;
type primary_colour = Red | Green | Blue

# [Red; Blue; Red];;
- : primary_colour list = [Red; Blue; Red]


# type http_response =
    | Data of string
    | Error_code of int;;
type http_response = Data of string | Error_code of int

# Data "<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <title>Dummy</title>
  </head>
  <body>
    Dummy Page
  </body>
</html>";;

- : http_response =
Data
 "<!DOCTYPE html>\n<html lang=\"en\">\n  <head>\n    <meta charset=\"utf-8\">\n    <title>Dummy</title>\n  </head>\n  <body>\n    Dummy Page\n  </body>\n</html>"

# Error_code 404;;
- : http_response = Error_code 404

# type page_range =
    | All
    | Current
    | Range of int * int;;
type page_range = All | Current | Range of int * int

```
23. 使用模式匹配时直接解包得到数据，以下是variant的用例
```ocaml
# let colour_to_rgb colour =
    match colour with
    | Red -> (0xff, 0, 0)
    | Green -> (0, 0xff, 0)
    | Blue -> (0, 0, 0xff);;
val colour_to_rgb : primary_colour -> int * int * int = <fun>

# let http_status_code response =
    match response with
    | Data _ -> 200
    | Error_code code -> code;;
val http_status_code : http_response -> int = <fun>

# let is_printable page_count cur range =
    match range with
    | All -> true
    | Current -> 0 <= cur && cur < page_count
    | Range (lo, hi) -> 0 <= lo && lo <= hi && hi < page_count;;
val is_printable : int -> int -> page_range -> bool = <fun>
```
24. 所有带有of的变体分支都是构造器，构造器的核心就是：**构造时如何组装数据，解构时就如何拆解数据** 构造和解构的操作是对称的，你如何构造出变体，在拆解时就如何匹配！比如下面的列表定义：
```ocaml
(* 原始定义 *)
type 'a list = [] | (::) of 'a * 'a list

(* 脱糖后等价于 *)
type 'a list = 
  | [] 
  | Cons of 'a * 'a list   (* 假设改名为Cons *)

(* 此时操作完全对称 *)
let lst = Cons (1, Cons (2, []))  (* 构造 *)
match lst with Cons (hd, tl) -> ... (* 解构 *)

```
25. 使用Record：结构体或者kv pair（字典），注意在实例化record时不需要写明record的类型！编译器会自动匹配格式相同的record定义，但这就导致了你不能定义record B， 其内容为reocrd A的内容再在尾部加上几个属性
	使用.property来访问record的属性
```ocaml
# type person = {
    first_name : string;
    surname : string;
    age : int
  };;
type person = { first_name : string; surname : string; age : int; }

# let gerard = {
     first_name = "Gérard";
     surname = "Huet";
     age = 76
  };;
val gerard : person = {first_name = "Gérard"; surname = "Huet"; age = 76}

# let s = gerard.surname;;
val s : string = "Huet"

# let is_teenager person =
    match person with
    | { age = x; _ } -> 13 <= x && x <= 19;;
val is_teenager : person -> bool = <fun>

# is_teenager gerard;;
- : bool = false

```
26. 错误处理：使用raise关键词抛出错误，使用try...with结构处理...中的错误，with后处理错误
```ocaml
# let id_42 n = if n <> 42 then raise (Failure "Sorry") else n;;
val id_42 : int -> int = <fun>

# id_42 42;;
- : int = 42

# id_42 0;;
Exception: Failure "Sorry".

# try id_42 0 with Failure _ -> 0;;
- : int = 0

```
27. 使用result type，分别对待Ok和Error
```ocaml

# let id_42_res n = if n <> 42 then Error "Sorry" else Ok n;;
val id_42_res : int -> (int, string) result = <fun>

# id_42_res 42;;
- : (int, string) result = Ok 42

# id_42_res 0;;
- : (int, string) result = Error "Sorry"

# match id_42_res 0 with
  | Ok n -> n
  | Error _ -> 0;;
- : int = 0

```
28. 使用begin-end标记语句块的开始与结束，而非使用大括号：
```ocaml
let z : int =
	let x = 3 in begin
	let y = x + x in begin
		(y * y) + x	
	end
end
```
29. 函数声明的两种方式
```ocaml
(* 更简便的写法，是第二种写法的语法糖 *)
let double x = x + x

(* 使用fun关键字定义匿名函数， ->操作符也是匿名函数定义要求的，定义完匿名函数后再将其绑定到double名称上 *)
let double : int -> int = fun (x: int) -> x + x
```
30. 检测list是否有序
```ocaml
let rec is_sorted (l : 'a list) : bool =

match l with
| [] -> true
| [ _ ] -> true
| h1 :: h2 :: tl -> h1 < h2 && is_sorted (h2 :: tl)
```
