(* Abstract Syntax Tree *)

type op = Add | Sub | Lt | Gt | Lte | Gte | Eq | Neq | 
          Or | And | Xor | Shl | Shr 

type uop = Not | Umin

type asn = Asn | Casn

type expr =
  | Num of int
  | Id of string   
  | Subbus of string * int * int
  | Unop of uop * expr
  | Binop of expr * op * expr
  | Basn of expr * asn * string
  | Subasn of expr * asn * string * int * int

type stmt =
  | Expr of expr
  | Call of expr list * string * expr list

type bus = { name : string; size : int; init : expr; isAsn : bool array } 

type gdecl = Const of bus * expr 
  (* ensure in semant that this expr is int *)

type vdecl = 
  | Bdecl of bus
  (* | Adecl of bus * int *)

type fbody =  vdecl list * stmt list

type fdecl = {
    portin  : bus list;
    fname   : string;
    portout : bus list;
    body    : fbody;
  }

type program = gdecl list * fdecl list

(* Pretty-printing functions *)

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Lt  -> "<"
  | Gt  -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | Eq  -> "=="
  | Neq -> "!="
  | Or  -> "|"
  | And -> "&"
  | Xor -> "^"
  | Shl -> "<<"
  | Shr -> ">>"

let string_of_uop = function
  | Not  -> "!"
  | Umin -> "-"

let string_of_asn = function
  | Asn  -> "->"
  | Casn -> "-:"

let rec string_of_expr = function
  | Num(l) -> string_of_int l
  | Id(s)  -> s
  | Subbus(n, i1, i2) ->
      n ^ "(" ^ 
      (if i2=i1+1 then string_of_int i1 
       else string_of_int i1 ^ ":" ^ string_of_int (i2-1)) 
      ^ ")"
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ 
      string_of_op o ^ " " ^ 
      string_of_expr e2
  | Basn(e, a, n) -> 
      string_of_expr e ^ " " ^
      string_of_asn a ^ " " ^
      n
  | Subasn(e, a, n, i1, i2) ->
      string_of_expr e ^ " " ^
      string_of_asn a ^ " " ^
      string_of_expr (Subbus(n, i1, i2))

let rec string_of_stmt = function
  | Expr(expr) -> string_of_expr expr ^ "\n"
  | Call(il, f, ol) ->
      "[" ^
      String.concat ", " (List.map string_of_expr il) ^ "] " ^ 
      f ^ " [" ^
      String.concat ", " (List.map string_of_expr ol) ^ "]"

let string_of_bus bus = 
  string_of_expr bus.init ^ " -> " ^
  "bit" ^ (if bus.size = 1 then "" else "." ^ string_of_int bus.size) ^
  " " ^ bus.name

let string_of_vdecl v = match v with
  | Bdecl bus -> string_of_bus bus

let string_of_gdecl v = match v with
  | Const(bus, s) -> string_of_bus bus ^ " const"

let string_of_fdecl fdecl = 
  String.concat ", " (List.map (fun b -> b.name) fdecl.portin) ^ " " ^
  fdecl.fname ^ 
  String.concat ", " (List.map (fun b -> b.name) fdecl.portout) ^ ":\n\t" ^
  String.concat "\n\t" (List.map string_of_vdecl (fst fdecl.body)) ^ "\n\t" ^
  String.concat "\t" (List.map string_of_stmt (snd fdecl.body))

let string_of_program (vars, funcs) =
  String.concat "\n" (List.map string_of_gdecl (List.rev vars)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
  