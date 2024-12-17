(* ast.ml *)
type prog = Block of block

and block =
  | EmptyBlock
  | CommandList of command * block
  
and command =
  | DeclareVar of var
  | FunctionCall of iden * expr
  | Malloc of var
  | Assignment of iden * expr
  | WhileLoop of boolExpr * block 
  | Conditional of boolExpr * block * block
  | Atomic of block
  | PrintExpr of expr
  | ParallelBlocks of block * block
  | SkipCommand

and boolExpr = 
  | Boolean of bool
  | Comparison of expr * boolOp * expr 
  
and expr =
  | Integer of int
  | ArithmeticExpr of expr * arithOp * expr 
  | Identifier of iden
  | NullValue
  | Procedure of var * block

and iden =
  | Var of var
  | Field of var * field 
  
and var = string

and field = string

and arithOp = 
  | Plus 
  | Minus 
  | Times 
  | Div

and boolOp = 
  | Lt 
  | Gt 
  | Leq 
  | Geq 
  | Eq

(* Function to append two block values *)
let rec block_append l1 l2 =
  match l1 with
  | EmptyBlock -> l2
  | CommandList (command, rest) -> CommandList (command, block_append rest l2) 