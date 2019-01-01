
(* Used for compiling *)

type action = Ast | Flatten | LLVM_IR | Compile

let _ =
let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the SAST");
    ("-f", Arg.Unit (set_action Flatten), "Print the flattened net list");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile), "Compile program");
  ] in
  let usage_msg = "usage: ./sandbox [-a|-f|-l|-c] [file.sb]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  let pc = Pre.process !channel in
  let lexbuf = Lexing.from_channel pc in
 	let ast = Parser.program Lexer.token lexbuf in
 	Semant.check ast;
 	match !action with
 		| Ast -> print_string (Ast.string_of_program ast)
 		| Flatten -> let (_,nl,_,_) = Flat.flatten ast in
  		print_string (Flat.string_of_netlist nl) 
 		| LLVM_IR -> 
 			print_string (Llvm.string_of_llmodule (Codegen.translate (Flat.flatten ast)))
 		| Compile -> let m = Codegen.translate (Flat.flatten ast) in
      Llvm_analysis.assert_valid_module m;
    	print_string (Llvm.string_of_llmodule m)