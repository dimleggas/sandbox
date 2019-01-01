
(*
	assumes the the .sb file is formatted correctly
	keeps track of indentation to form blocks
	adds semi-colons
	will not keep track if inside comments
*)

open Printf

let process ic =
	let out_file = "pre.txt" in
	let oc = open_out out_file in
	let rec read_lines l =
		try read_lines ((input_line ic)::l) 
		with End_of_file -> close_in ic; l
	in
	
	let process_line t l = 
		if l = "" then t
		else if l.[String.length l - 1] = '/' then (fprintf oc "%s\n" l; t)
		else if l.[0] = '\t' then (fprintf oc "%s;\n" l; 1)
		else (
			let tokens = String.split_on_char ' ' l 
			and opt = if t = 1 then "}\n" else "" in
			match List.rev tokens with
				| [] -> fprintf oc ""; t (* never matched *)
				| hd::tl -> let last = hd and others = List.rev tl in
					let ll = String.length last in
					if ll > 0 then 
						(* function header *)
						if last.[ll-1] = ':' then (
						fprintf oc "%s%s %s{\n"
						opt
						(String.concat " " others)
						(String.sub last 0 (ll-1)); 1)
						(* then global decl *)
						else (fprintf oc "%s%s;\n" opt l; 0)
					else (fprintf oc "%s%s;\n" opt l; 0)
		) 
	in let lines = List.rev(read_lines [])
	in ignore(List.fold_left process_line 0 lines);
	fprintf oc "}\n";
	close_out_noerr oc;
	open_in out_file
	