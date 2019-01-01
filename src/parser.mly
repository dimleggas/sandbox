/* parser for sandbox */

%{ open Ast %}

/* tokens */
%token PLUS MINUS OR AND XOR SHL SHR
%token LT GT LTE GTE EQ NEQ
%token NOT 
%token ASSIGN CLKASN WIRE
%token COMMA SEMI COLON CAT DOT 
%token CONST BIT
%token OPAREN CPAREN OBRACK CBRACK OBRACE CBRACE
%token <int> NUM
%token <string> ID
%token EOF

/* precedence */
%left COMMA SEMI
%right ASSIGN CLKASN
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS 
%left OR
%left XOR
%left AND
%left SHL SHR
%right NOT UMIN

%start program
%type <Ast.program> program

%%

program: decls EOF { $1 }

decls: 
  | /* nothing */  { [], [] }
  | decls gdecl    { ($2 :: fst $1), snd $1 }
  | decls fdecl    { fst $1, ($2 :: snd $1) }

gdecl:
  | bdecl CONST SEMI  { Const($1, $1.init) }

bdecl:
  | init_opt BIT size_opt ID  
      { { 
        name = $4;
        size = $3;
        init = $1;
        isAsn = Array.make $3 false
      } }  

init_opt:
  | /* nothing */       { Num 0 }
  | expr assign         { $1 }

size_opt:
  | /* nothing */       { 1 }
  | DOT NUM             { $2 }


fdecl:
  OPAREN port CPAREN ID OPAREN port_out CPAREN 
  OBRACE fbody CBRACE 
    { { 
      portin  = $2;
      fname   = $4;
      portout = $6;
      body    = List.rev (fst $9), List.rev (snd $9);
    } }

port:
  | /* nothing */      { [] } 
  | busses             { List.rev $1 }

port_out:
  | /* nothing */      { raise (Failure("Empty output port list")) }
  | busses             { List.rev $1 }

busses:
  | bdecl                   { [$1] }
  | busses COMMA bdecl      { $3 :: $1 }

fbody:
  | /* nothing */      { [], [] }
  | fbody local        { ($2 :: fst $1), snd $1 }
  | fbody stmt         { fst $1, ($2 :: snd $1) }

local:
  | vdecl SEMI         { $1 }

vdecl:
  | bdecl          { Bdecl($1) }

stmt:
  | asnexpr SEMI                { Expr $1 }
  | OBRACK actuals CBRACK ID OBRACK actuals CBRACK SEMI
      { Call($2, $4, $6) }

asnexpr:
  | expr assign ID   { Basn($1, $2, $3) }
  | expr assign ID OPAREN NUM COLON NUM CPAREN  
      { Subasn($1, $2, $3, $5, $7) }
  | expr assign ID OPAREN NUM CPAREN  
      { Subasn($1, $2, $3, $5, $5+1) }

assign:
  | ASSIGN              { Asn }
  | CLKASN              { Casn }

actuals:
  | /* nothing */       { [] }
  | actual_list         { List.rev $1}

actual_list:
  | expr                   { [$1] }
  | actual_list COMMA expr { $3 :: $1 }

expr:
  | NUM               { Num($1) }
  | ID                { Id($1) }
  | ID OPAREN NUM COLON NUM CPAREN { Subbus($1, $3, $5) }
  | ID OPAREN NUM CPAREN           { Subbus($1, $3, $3+1) }
  /* ADD CAT */
  | MINUS expr %prec UMIN     { Unop(Umin, $2) }
  | NOT   expr %prec NOT      { Unop(Not, $2)  }
  | expr PLUS  expr   { Binop($1, Add, $3) }
  | expr MINUS expr   { Binop($1, Sub, $3) }
  | expr EQ    expr   { Binop($1, Eq, $3)  }
  | expr NEQ   expr   { Binop($1, Neq, $3) }
  | expr LT    expr   { Binop($1, Lt, $3)  }
  | expr LTE   expr   { Binop($1, Lte, $3) }
  | expr GT    expr   { Binop($1, Gt, $3)  }
  | expr GTE   expr   { Binop($1, Gte, $3) }
  | expr AND   expr   { Binop($1, And, $3) }
  | expr OR    expr   { Binop($1, Or, $3)  }
  | expr XOR   expr   { Binop($1, Xor, $3) }
  | expr SHL   expr   { Binop($1, Shl, $3) }
  | expr SHR   expr   { Binop($1, Shr, $3) }
  | OPAREN expr CPAREN        { $2 }
