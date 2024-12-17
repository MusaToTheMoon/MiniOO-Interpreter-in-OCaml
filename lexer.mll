(* lexer.mll *)
{
open Parser;;
exception Eof;;
exception Failure of string;;
}

let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = ['a'-'z' 'A'-'Z']
let var_identifier = lowercase (letter | digit | '_')*
let field_identifier = uppercase (letter | digit | '_')*

rule token = parse
  | "(*" { skip_comment lexbuf; token lexbuf }
  | whitespace { token lexbuf }

  (* Keywords *)
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "null" { NULL }
  | "proc" { PROC_DECL }
  | "var" { VAR_DECL }
  | "malloc" { MALLOC }
  | "skip" { SKIP }
  | "while" { WHILE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "|||" { PARALLEL }
  | "atom" { ATOM }
  | "print" { PRINT }

  (* Operators and Delimiters *)
  | "==" { EQUALITY }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | '<' { LT }
  | '>' { GT }  
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '=' { ASSIGN }
  | '.' { DEREFERENCE }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }  
  | '/' { DIV }  
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPAREN }
  | ')' { RPAREN }  

  (* Identifiers *)
  | var_identifier as id { VAR_ID id }
  | field_identifier as id { FIELD_ID id }

  (* Integer Literals *)
  | digit+ as i { INT (int_of_string i) }

  (* End of File *)
  | eof { EOF }

  (* Unexpected Characters *)
  | _ as c { raise (Failure ("Lexer failed. Unknown character: " ^ (String.make 1 c) ^ 
                            Printf.sprintf " At offset %d.\n" (Lexing.lexeme_start lexbuf))) }

and skip_comment = parse
  | "*)" { () }
  | "(*" { skip_comment lexbuf; skip_comment lexbuf } (* To handle nested comments *)
  | _ { skip_comment lexbuf }

{

}