(* Semantic Checking *)

open Ast

module StringMap = Map.Make(String)

(*** HELPER FUNCTIONS ***)

(* raise failure if duplicates exist *)
let report_duplicate exceptf list = 
	let rec helper = function 
		| n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
		| _ :: t -> helper t
		| [] -> ()
   in helper (List.sort compare list)

(* give underlying bus of declarations *)
let gdec2b d = match d with Const(b, s) -> b 
let vdec2b d = match d with Bdecl b -> b 

(* number of bits required to describe int x *)
let bit_required x =
	(* for the moment assuming x > 0 tho *)
	let x = abs x
	in let log2 y = 
		int_of_float ( ((log (float_of_int y)) /. (log 2.)) )
	in (log2 x) + 1

(* raise failure if some element not equal to another *)
let all_eq l =
	let rec diff d = function
		| [] | [_] -> d
		| hd::tl -> diff ((hd - List.nth tl 0)::d) tl
	in let diffs = diff [] l
	in if not (List.for_all (fun x -> x = 0) diffs) then
		raise (Failure("invalid arguments")) else ()

(* number of bits required for result of binop *)
let binop_size s1 op s2 = match op with
	| And | Or | Xor -> if s1 != s2
		then raise(Failure("operand sizes do not match "))
		else s1
	| Add | Sub -> Pervasives.max s1 s2
	| Shl | Shr -> s1
	| Lt | Gt | Lte | Gte | Eq | Neq -> 1

(*** CHECK THAT THE AST IS SEMANTICALLY CORRECT ***)

(* function for checking a single assign *)
(* use x and y to be usable for subbus *)
let check_basn e es b x y = ( match e with
	| Num _ -> if es > y-x then raise(Failure("size mismatch in " ^ b.name))
		else ()
	| Id _ | Subbus(_,_,_) | Unop(_,_) | Binop(_,_,_) -> if es != y-x
		then raise(Failure("size mismatch in " ^ b.name)) else ()
	| _ -> raise (Failure("illegal bus assignment: " ^ b.name)) );
	for i = x to y-1 do if b.isAsn.(i)
		then raise (Failure("bus " ^ b.name ^ " has more than one driver"))
		else b.isAsn.(i) <- true 
	done
		
(* check if valid subbus *)
let check_subbus b x y = 
	if x >= 0 && y <= b.size && x < y then ()
	else raise(Failure("incorrect dereference of " ^ b.name)) 
		 
let check_subasn e es b x y = 
	check_subbus b x y;
	check_basn e es b x y

(* main checking function *)
let check (globaldecls, functions) = 
	(* checking globals *)
	let globals = List.map gdec2b globaldecls in
	(* no duplicate globals *)
	report_duplicate (fun n -> "duplicate global variable " ^ n) 
	(List.map (fun g -> g.name) globals);
	(* globals intialized to an int *)
	let check_global_init g = match g.init with
		| Num _ -> ()
		| _ -> raise (Failure ("global " ^ g.name ^ " must be initialized to an integer"))
	in List.iter check_global_init globals;

	(* checking functions *)
	(* no duplicate functions *)
	report_duplicate (fun n -> "duplicate function " ^ n)
		(List.map (fun fd -> fd.fname) functions);

	(* collect declared functions *)
	let function_decls = List.fold_left 
		(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty functions
	in
	let function_decl s = try StringMap.find s function_decls
	with Not_found -> raise (Failure ("no function " ^ s))
	in
	(* ensure that sandbox defined *)
	let _ = function_decl "sandbox" 
	in
	(* check each function decl *)
	let check_function func = 
		(* ensure no conflict between portin/portout/locals *)
		let locals = func.portin @ func.portout @ (List.map vdec2b (fst func.body)) 
		in report_duplicate 
		(fun n -> "duplicate in/out/local bus " ^ n ^ " in " ^ func.fname) 
		(List.map (fun b -> b.name) locals);
		(* build symbol table for all busses visible in function *)
		let symbols = List.fold_left (fun m b -> StringMap.add b.name b m)
		StringMap.empty (globals @ locals)
		in
		let lookup s = 
	    	try StringMap.find s symbols
	  		with Not_found -> raise (Failure ("undeclared indentifier " ^ s))
		in

		(* need to ensure all outputs are assigned *)
		let out_names = List.map (fun b -> b.name) func.portout in
		let out_table = Hashtbl.create (2 * List.length out_names) in
		List.iter (fun out -> Hashtbl.add out_table out false) out_names;

		let check_const n = 
			if List.mem n (List.map (fun b->b.name) globals) &&
			not (List.mem n (List.map (fun b->b.name) globals))
			then raise(Failure("cannot change const")) else ()
		in
		(* returns number of bits required for expression *)
		let rec expr = function
			| Num x -> bit_required x
			| Id s -> (lookup s).size
			| Subbus(n, i1, i2) -> let b = lookup n in
					check_subbus b i1 i2; i2-i1
			| Unop(op, e) -> expr e
			| Binop(e1, op, e2) -> let s1 = expr e1 and s2 = expr e2 in
					binop_size s1 op s2
			| Basn(e, a, n) -> let s = expr e and b = lookup n in
					check_const n;
					check_basn e s b 0 b.size; 
					if List.mem n out_names then Hashtbl.replace out_table n true; 
					b.size
			| Subasn(e, a, n, i1, i2) -> let s = expr e and b = lookup n in
					check_const n;
					check_subasn e s b i1 i2;
					if List.mem n out_names then Hashtbl.replace out_table n true; 
					i2-i1
		in 
		(* returns unit if semantically valid *)
		let rec stmt = function
			| Expr e -> ignore(expr e)
			| Call(inputs, n, outputs) -> let fd = function_decl n in
				(* can only assign to busses and subbuses *)
				List.iter (fun out -> match out with
					| Id _ | Subbus(_,_,_) -> ()
					| _ -> raise(Failure("only bus or subbus can be an output"))
				) outputs;
				(* calls to sandbox are not permitted  *)
				if fd.fname = "sandbox" then raise(Failure("cannot call sandbox"))
				(* do number of actuals/outputs match portin/portout *)
				else if (List.length inputs) != (List.length fd.portin)
					then raise(Failure("input mismatch in " ^ n))
				else if (List.length fd.portout) != (List.length outputs)
					then raise(Failure("output mismatch in " ^ n))
				(* can inputs fit in portin and outputs in portout *)
				(* accounts for shorthand function calls... *)
				else
					let check_ports acts port =
						List.iter2 (fun x y -> if not (x mod y = 0) then
							raise (Failure("invalid arguments")) else ())
						acts port;
						let quo = List.map2 (fun x y -> x / y) acts port
						in all_eq quo
					in
					check_ports 
						(List.map expr inputs) (List.map (fun b -> b.size) fd.portin);
					check_ports 
						(List.map expr outputs) (List.map (fun b -> b.size) fd.portout);

					(* sizes accounted for, but can outputs be assigned? *)
					let check_outasn o = match o with
						| Id s -> let out = lookup s in
							check_basn (Id "dummy") out.size out 0 out.size;
							if List.mem out.name out_names 
								then Hashtbl.replace out_table out.name true
						| Subbus(n, i1, i2) -> let out = lookup n in
							check_subasn (Id "dummy") (i2-i1) out i1 i2;
							if List.mem out.name out_names 
								then Hashtbl.replace out_table out.name true
						| _ -> raise (Failure(n ^ " cannot port to these outputs ")) 
					in List.iter check_outasn outputs

		(* check each statement and that all outputs are assigned *)
		in List.iter stmt (snd func.body);
		Hashtbl.iter (fun n x -> if x then () 
			else raise(Failure("not all outputs of " ^ func.fname ^ " assigned"))) 
		out_table
	in 
	List.iter check_function functions

(* BREAK UP BUSSES INTO BITS *)
(* THE CODE BELOW HERE CAUSES NO ERRORS BUT IS NOT
	 COMPLETE. THE INTENT WAS TO RETURN A MODIFIED AST
	 WHERE EVERYTHNG HAS BEEN BROKEN INTO BITS
 *)

(* convert decimal number to list of bits of given len *)
let d2b x len =
	let rec dec2bin y lst = match y with 
		| 0 -> (List.rev lst) 
			@ (Array.to_list (Array.make (len - List.length lst) 0))
		| _ -> dec2bin (y / 2) ((y mod 2)::lst)
	in dec2bin x [] 

(* convert name into list of bit names *)
let n2b n i1 i2 =
	let rec name2bits n i lst = 
		if i = i1 then (n ^ "_" ^ (string_of_int i1))::lst
		else name2bits n (i-1) ((n ^ "_" ^ (string_of_int i))::lst)
	in name2bits n (i2-1) []

(* break unops up into bitwise unops *)
let break_unop uop ex = match uop with
	| Not -> []
	| Umin -> []

(* break binops up into bitwise binops *)
let break_binop e1 op e2 = []

(* function for breaking a single assign *)
(* use x and y to be usable for subbus *)
let break_basn e a b x y = ( match e with
	| Num v -> List.map2 (fun q p -> Basn(Num q, a, p)) 
			(d2b v y) (n2b b.name x y)
	| Id s -> List.map2 (fun q p -> Basn(Id q, a, p)) 
			(n2b s 0 y) (n2b b.name x y)
	| Subbus(n,i1,i2) -> List.map2 (fun q p -> Basn(Id q, a, p)) 
			(n2b n i1 i2) (n2b b.name x y)
	| Unop(uo,e1) -> []
	| Binop(e1,o,e2) -> [] 
	| _ -> raise (Failure("never reached")) )

let break_busses gb =
	let binit = match gb.init with Num x -> x | _ -> 0 in
	let vals = d2b binit gb.size in 
	let nams = n2b gb.name 0 gb.size in 
	List.map2 (fun n v -> 
		{ 
			name = n;
			size = 1;
			init = Num v;
			isAsn = [| false |]
		}
	) nams vals

let break (globaldecls, functions) = 
	(* break up globals *)
	let globals = List.map gdec2b globaldecls in
	let broken_globals = List.concat (List.map break_busses globals) in
	
	(* collect functions *)
	(* let function_decls = List.fold_left 
		(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty functions
	in
	let function_decl s = StringMap.find s function_decls
	in *)
	(* check each function decl *)
	let break_function func = 
		(* ensure no conflict between portin/portout/locals *)
		let locals = func.portin @ func.portout @ (List.map vdec2b (fst func.body)) in 
		(* let broken_locals = List.concat (List.map break_busses locals) in *)

		(* build symbol table for all busses visible in function *)
		let symbols = List.fold_left (fun m b -> StringMap.add b.name b m)
		StringMap.empty (globals @ locals)
		in let lookup s = StringMap.find s symbols
		in
		(* list of stmts on bits *)
		let rec break_stmt = function
			| Expr e -> (match e with
				| Basn(ex,a,nb) -> let b = lookup nb in 
						break_basn ex a b 0 b.size
				| Subasn(ex,a,nb,i1,i2) -> let b = lookup nb in 
						break_basn ex a b i1 i2
				| _ -> []
			)
			| Call(inputs, n, outputs) -> [] (* let fd = function_decl n in [] *)
				
		(* check each statement and that all outputs are assigned *)
		in List.map break_stmt (snd func.body)
	
	in 
	broken_globals, List.map break_function functions
