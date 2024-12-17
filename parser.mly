(* parser.mly *)
%{
open Ast
%}

(* tokens *)
%token EOF NULL PROC_DECL VAR_DECL MALLOC SKIP WHILE IF THEN ELSE PARALLEL ATOM PRINT LBRACE RBRACE
%token EQUALITY LEQ GEQ LT GT
%token COLON SEMICOLON ASSIGN DEREFERENCE COMMENT_BEGIN COMMENT_END
%token PLUS MINUS TIMES DIV LPAREN RPAREN 
%token <string> VAR_ID FIELD_ID
%token <int> INT
%token <bool> BOOL

%start prog
%type <Ast.prog> prog  
%type <Ast.block> cmds
%type <Ast.command> cmd
%type <Ast.expr> expr
%type <Ast.iden> iden
%type <Ast.boolExpr> boolean

(* associativity and precendence *)
%left PLUS MINUS          (* lowest precedence  *)
%left TIMES DIV           (* highest precedence *)
%nonassoc UMINUS

(* rules *) 
%% 

prog :
  l=cmds EOF { Block l }

cmds :
  | c=cmd SEMICOLON l=cmds { CommandList (c, l) }
  | LBRACE l1=cmds RBRACE l2=cmds { block_append l1 l2 }
  |  { EmptyBlock }

cmd :
  | VAR_DECL v=VAR_ID { DeclareVar v }
  | i=iden LPAREN e=expr RPAREN { FunctionCall (i, e) }
  | MALLOC LPAREN v=VAR_ID RPAREN { Malloc v }
  | i=iden ASSIGN e=expr { Assignment (i,e) }
  | WHILE LPAREN b=boolean RPAREN l=cmds { WhileLoop (b, l) }
  | WHILE b=boolean l=cmds { WhileLoop (b, l) }
  | IF LPAREN b=boolean RPAREN THEN l1=cmds ELSE l2=cmds { Conditional (b, l1, l2) }
  | IF b=boolean THEN l1=cmds ELSE l2=cmds { Conditional (b, l1, l2) }
  | IF LPAREN b=boolean RPAREN THEN l1=cmds { Conditional (b, l1, EmptyBlock) }
  | IF b=boolean THEN l1=cmds { Conditional (b, l1, EmptyBlock) }
  | ATOM LPAREN l=cmds RPAREN { Atomic l }
  | LBRACE l1=cmds PARALLEL l2=cmds RBRACE { ParallelBlocks (l1, l2) }
  | PRINT LPAREN e=expr RPAREN { PrintExpr e }
  | SKIP { SkipCommand }

boolean :
  | b=BOOL {Boolean b}
  | e1=expr EQUALITY e2=expr { Comparison (e1, Eq, e2) }
  | e1=expr LT e2=expr { Comparison (e1, Lt, e2) }
  | e1=expr GT e2=expr { Comparison (e1, Gt, e2) }
  | e1=expr LEQ e2=expr { Comparison (e1, Leq, e2) }
  | e1=expr GEQ e2=expr { Comparison (e1, Geq, e2) }

expr :
  | LPAREN e=expr RPAREN { e }
  | n=INT { Integer n }
  | MINUS e=expr %prec UMINUS { match e with
    | Integer n -> Integer (-n)
    | _ -> raise (Failure "Parsing error. - only applies to integers")
    }
  | e1=expr DIV e2=expr { ArithmeticExpr (e1, Div, e2) }
  | e1=expr TIMES e2=expr { ArithmeticExpr (e1, Times, e2) }
  | e1=expr PLUS e2=expr { ArithmeticExpr (e1, Plus, e2) }
  | e1=expr MINUS e2=expr { ArithmeticExpr (e1, Minus, e2) }
  | i=iden { Identifier i }
  | NULL { NullValue }
  | PROC_DECL v=VAR_ID COLON l=cmds { Procedure (v, l) }

iden :
  | v=VAR_ID { Var v }
  | v=VAR_ID DEREFERENCE f=FIELD_ID { Field (v, f) }