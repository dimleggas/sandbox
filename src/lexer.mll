(* Lexical analyzer *)

{ open Parser }

rule token = parse 
  |[' ' '\t' '\r' '\n'] {token lexbuf} (* eat whitespace *)
  
  (* binary operators *)
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '|'              { OR }
  | '&'              { AND }
  | '^'              { XOR }
  | "<<"             { SHL }
  | ">>"             { SHR }
  | '<'              { LT }
  | '>'              { GT }
  | "<="             { LTE }
  | ">="             { GTE }
  | "=="             { EQ }
  | "!="             { NEQ }

  (* unary operator (also handle minus) *)
  | '!'              { NOT }

  (* other operators *)
  | "::"             { CAT }
  | '.'              { DOT }

  (* assignments *)
  | "->"			 { ASSIGN }
  | "-:"             { CLKASN }

  (* delimiters *)
  | ','              { COMMA }
  | ';'              { SEMI }
  | ':'              { COLON }

  (* scoping *)
  | '('              { OPAREN }
  | ')'              { CPAREN }
  | '['              { OBRACK }
  | ']'              { CBRACK }
  | '{'              { OBRACE }
  | '}'			     { CBRACE }

  (* key words *)
  | "const"          { CONST }
  | "bit"            { BIT }
  
  (* integer and string literals *)
  | ['0'-'9']+ as n  { NUM(int_of_string n) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as i { ID(i) }

  (* Comments, unrecognized, and EOF *)
  |"/"               {comment lexbuf}
  | _                { raise (Failure("illegal character")) }
  | eof              { EOF }

and comment = parse
  | "/"          {token lexbuf}
  | eof          { raise (Failure("comment started but never finished")) }
  | _            {comment lexbuf} 
  