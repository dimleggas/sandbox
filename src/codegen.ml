
(* Code Generation *)

module L = Llvm
module A = Ast
module F = Flat

module StringMap = Map.Make(String)

let translate (gl, nl, pi, po) =
	(* setup context / module *)
	let context = L.global_context () in
  let the_module = L.create_module context "Sandbox"
  		and i32_t  = L.i32_type   context
  		and void_t = L.void_type context
	in
	let intyps = 
		Array.of_list( [ L.pointer_type i32_t; L.pointer_type i32_t; i32_t ] ) in
	let mtyp = L.function_type void_t intyps in
	let main = L.define_function "sandbox" mtyp the_module in
	let builder = L.builder_at_end context (L.entry_block main) in

	(* declare globals *)
	List.iter
	(fun (n, i) -> 
		ignore(L.define_global n (L.const_int i32_t i) the_module))
	gl;

	(* outputs of flipflops  *)
	let clock_asn = 
		let rec get_clock_asn fl = function
			| [] | [_] -> fl
			| hd::tl -> if (List.nth tl 0) = F.As(Casn) 
				then get_clock_asn (hd::fl) tl else get_clock_asn fl tl 
		in get_clock_asn [] nl
	in 
	(* declare things that depend on state as static *)
	let clock_vars =
		let add_clock_variable m n = 
			let static0 = L.define_global (n ^ "__0") (L.const_int i32_t 0) the_module 
			and static1 = L.define_global (n ^ "__1") (L.const_int i32_t 0) the_module
			in 
			L.set_linkage L.Linkage.Internal static0; 
			L.set_linkage L.Linkage.Internal static1;
	   	StringMap.add n (static0, static1) m 
		in
  	List.fold_left 
  	(fun m n -> match n with 
  		| F.Var s -> add_clock_variable m s
  		| _ -> m (* never matched *)
  	) StringMap.empty clock_asn
  in
  let clock_lookup n state = let v = StringMap.find n clock_vars
  	in if state = 0 then fst v else snd v
  in 

	(* declare formals *)
	let vars =
   	let add_formal m n p = L.set_value_name n p;
			let local = 
				L.build_alloca 
				(if n = "sopt" then i32_t else L.pointer_type i32_t) 
				n builder 
   			in
			ignore (L.build_store p local builder);
			StringMap.add n local m 
   	in
   	(* define variables not on clock  *)
   	let add_variable m n =
	   	let var = L.build_alloca i32_t n builder
	   	in StringMap.add n var m 
		in
		(* add arguments *)
		let portin = ["input"; "output"; "sopt"] in
		let formals = List.fold_left2 add_formal StringMap.empty portin
    	(Array.to_list (L.params main)) 
		in
  	List.fold_left 
  	(fun m n -> match n with 
  		| F.Var s -> if not ( StringMap.mem s m || StringMap.mem s clock_vars )
  			then add_variable m s else m
  		| _ -> m
  	) formals nl (* extract variable names from nl *)
  in
	(* Return the value for a variable or formal argument *)
	let lookup n = StringMap.find n vars
	in
	(* get the state option *)
	let sopt = lookup "sopt" in
	(* load inputs  *)
	for i = 0 to ((List.length pi) - 1 ) 
		do 
			let arr = L.build_load (lookup "input") "input" builder in
			let index = L.const_int i32_t i in 
			let ptr = L.build_in_bounds_gep arr [| index |] "" builder in
			let inp = L.build_load ptr ("in"^(string_of_int i)) builder in
			ignore(L.build_store inp (lookup (List.nth pi i)) builder)
	done;
	(* compute llvalue of flattened netlist and store outputs *)
	let rec netlist s = function
		| [] ->
			(* store outputs *)
			for i = 0 to ((List.length po) - 1 ) 
			do 
				let v = List.nth po i in
				let state = if sopt = L.const_int i32_t 0 then 0 else 1 in
				let vv = if StringMap.mem v clock_vars 
					then clock_lookup v state
					else lookup v
				in 
				let out = L.build_load vv v builder in
				let arr = L.build_load (lookup "output") "output" builder in
				let index = L.const_int i32_t i in 
				let ptr = L.build_in_bounds_gep arr [| index |] "" builder in
				ignore(L.build_store out ptr builder)
			done;
			(* need to construct the return statement *)
			ignore(L.build_ret_void builder); builder
		(* lookup_global name m *)
		| n::tl -> Stack.push (match n with
			| F.Val i -> L.const_int i32_t i
			| F.Var v -> 
				if StringMap.mem v clock_vars then (
					let state = (if sopt = L.const_int i32_t 0 then "__0" else "__1") in
					let name = v ^ state in
				  let vv = match L.lookup_global name the_module with
				  	| Some llv -> llv
				  	| _ -> raise(Failure("never reached"))
					in 
				  (* if (List.mem v po) then (print_endline "1"; vv )else (print_endline "2"; L.build_load vv name builder) *)
				  if List.nth tl 0 = As(Casn) then vv else L.build_load vv name builder

				) else(
					(* if List.mem v po then lookup v  *)
					if List.nth tl 0 = As(Asn) then lookup v
					else L.build_load (lookup v) v builder
				)
			| F.Uo uo -> let n1 = Stack.pop s in
				(match uo with
				| A.Umin -> L.build_neg
				| A.Not  -> L.build_not
				) n1 "" builder	
			| F.Op op -> let n2 = Stack.pop s and n1 = Stack.pop s in 
				(match op with
				| A.Add -> L.build_add n1 n2 "" builder
				| A.Sub -> L.build_sub n1 n2 "" builder
				| A.Lt  -> L.build_icmp L.Icmp.Slt n1 n2 "" builder
				| A.Gt  -> L.build_icmp L.Icmp.Sgt n1 n2 "" builder
				| A.Lte -> L.build_icmp L.Icmp.Sle n1 n2 "" builder
				| A.Gte -> L.build_icmp L.Icmp.Sge n1 n2 "" builder
				| A.Eq  -> L.build_icmp L.Icmp.Eq  n1 n2 "" builder
				| A.Neq -> L.build_icmp L.Icmp.Ne  n1 n2 "" builder
				| A.Or  -> L.build_or  n1 n2 "" builder
				| A.And -> L.build_and n1 n2 "" builder
				| A.Xor -> L.build_xor n1 n2 "" builder
				| A.Shl -> L.build_shl n1 n2 "" builder
				| A.Shr -> L.build_lshr n1 n2 "" builder
				)
			| F.As a -> let n2 = Stack.pop s and n1 = Stack.pop s in 
				L.build_store n1 n2 builder
		) s; 
		netlist s tl
	in

	let (empty_stack : L.llvalue Stack.t) = Stack.create () 
	in
	ignore(netlist empty_stack nl);
	the_module