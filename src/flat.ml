
(* Flattening stage *)

open Ast

type node = 
	| Val of int 
	| Var of string 
	| Uo of uop 
	| Op of op
	| As of asn

module StringMap = Map.Make(String)

let flatten (globaldecls, functions) = 
	(* globals busses *)
	let g2b d = match d with Const(b, s) -> b in
	let globals = List.map g2b globaldecls in
	(* let global_names = List.map (fun b -> b.name) globals in *)
	let globes = 
		List.map 
		(fun g -> match g.init with
			| Num x -> g.name, x 
			| _ -> raise(Failure("never reached")) )
	 	globals
	in
	(* table keeping track of variable names *)
	let var_table = Hashtbl.create 100 in

	(* function declarations *)
	let function_decls = List.fold_left 
  	(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty functions
	in
	let func_lookup n = StringMap.find n function_decls in
	let sbd = func_lookup "sandbox" 
	in
	let rec f2g f inexpr outexpr = 
		(* local busses *)
		let d2b d = match d with Bdecl b -> b in
		let ldec = List.map d2b (fst f.body) in
		let locals = f.portin @ f.portout @ ldec in
		(* for keeping track of naming *)
		let track_name b = let n = b.name in
			if Hashtbl.mem var_table n 
			then Hashtbl.replace var_table n (Hashtbl.find var_table n + 1)
			else Hashtbl.add var_table n 0
		in List.iter track_name locals;
		let get_local n = 
			n ^ "_" ^ string_of_int (Hashtbl.find var_table n) in
		(* all available busses *)
		let loces = 
			List.concat 
			(List.map 
			(fun l -> match l.init with
				| Num x -> [Val x; Var(get_local l.name); As(Asn)]
				| _ -> raise(Failure("never reached")) )
	 		ldec)
		in
		(* symbols only contains locals, globals in globals  *)
		let symbols = List.fold_left (fun m b -> StringMap.add b.name b m)
			StringMap.empty locals
		in
		(* mapping formals to actuals *)
		let f2a = 
			let formals = List.map (fun b -> b.name) f.portin in
			List.fold_left2 (fun m f a -> StringMap.add f a m) 
			StringMap.empty formals inexpr
		in
		(* mapping formals to outputs *)
		let f2o = 
			let formals = List.map (fun b -> b.name) f.portout in
			List.fold_left2 (fun m f o -> StringMap.add f o m) 
			StringMap.empty formals outexpr
		in
		let rec expr2g = function
			| Num i -> [Val i]
			| Id s -> (* is it a local? *)
				if StringMap.mem s symbols then 
					(if StringMap.mem s f2a
					then [Var(StringMap.find s f2a)] (* is it a portin? *)
					else [Var(get_local s)]) (* or just a normal local? *)
				else [Var s] (* or a global *)
			| Unop(o, e) -> (expr2g e) @ [Uo o] 
			| Subbus(n,e1,e2) -> []
			| Binop(e1, o, e2) -> (expr2g e1) @ (expr2g e2) @ [Op o]
			| Basn(e, a, n) -> let store = 
					(if StringMap.mem n f2o
						then [Var(StringMap.find n f2o)] (* is it a portout? *)
					else [Var(get_local n)]) (* or just a normal local? *)
				in (expr2g e) @ store @ [As a]
			| Subasn(e, a, n, i1, i2) -> []
		in
		let rec stmt2g g = function
			| Expr e -> g @ (expr2g e)
			| Call(ins, fn, outs) -> 
				(* let x = List.concat (List.map expr2g ins)
				and y = List.concat (List.map expr2g outs) *)
				let x = List.map 
				(fun a -> match a with Id s -> get_local s | _ -> raise(Failure("not handled yet"))) 
				ins
				and y = List.map 
				(fun a -> match a with Id s -> get_local s | _ -> raise(Failure("not handled yet"))) 
				outs
				in g @ (f2g (func_lookup fn) x y)
		in
		loces @ (List.fold_left stmt2g [] (snd f.body)) 

	(* flatten sandbox, and thus the program *)
	in let pi = List.map (fun b -> b.name ^ "_0") sbd.portin 	
	in let po = List.map (fun b -> b.name ^ "_0") sbd.portout

	(* in let circ_in = List.map (fun n -> Var(n)) pi
	in let circ_out = List.map (fun n -> Var(n)) po *)
	in
	( globes,
		f2g sbd pi po, 
		pi, 
		po  )

(* Pretty-printing functions *)

let string_of_node = function
	| Val(i) -> string_of_int i
	| Var(s) -> s 
	| Uo(u)  -> string_of_uop u
	| Op(o)  -> string_of_op o
	| As(a)  -> string_of_asn a

let rec string_of_netlist = function
	| [] -> "\n"
	| n::tl -> string_of_node n ^ " " ^ string_of_netlist tl

	