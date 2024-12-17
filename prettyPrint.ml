(* prettyPrint.ml *)
open Ast;;
open Stack;;
open Printf;;
open OperationalTypes;;

(* ============== Helper functions ============ *)

(* Helper functions for colored output *)
let red s = "\x1b[31m" ^ s ^ "\x1b[0m"    (* Red color *)
let green s = "\x1b[32m" ^ s ^ "\x1b[0m"  (* Green color *)
let white s = "\x1b[37m" ^ s ^ "\x1b[0m"  (* White color *)

let string_of_list string_of_function l =
  let rec aux acc l = match l with
    | h :: t -> 
        let element = string_of_function h in
        let sep = if t = [] then "" else ",\n" in
        aux (acc ^ element ^ sep) t
    | [] -> acc ^ "]"
  in
  "[" ^ aux "" l
  
let print_indent indent is_last =
  for i = 0 to indent -2 do
    Printf.printf "|   "
  done;
  if indent > 0 then
    Printf.printf (if is_last then "└── " else "├── ")

let rec pretty_print_prog (Block commands) =
  Printf.printf "%s\n" (white "ABSTRACT SYNTAX TREE:");
  pretty_print_block commands 1 true

and pretty_print_block block indent is_last =
  match block with
  | EmptyBlock -> ()
  | CommandList (command, EmptyBlock) ->
      pretty_print_command command indent true
  | CommandList (command, remaining) ->
      pretty_print_command command indent false;
      pretty_print_block remaining indent is_last

and pretty_print_command command indent is_last =
  print_indent indent is_last;
  match command with
  | DeclareVar var ->
      Printf.printf "%s: %s\n" (red "Declare") (green var)
  | Assignment (iden, expr) ->
      Printf.printf "%s\n" (red "Assign");
      (* Identifier node *)
      print_indent (indent + 1) false;
      Printf.printf "%s: %s\n" (red "Identifier") (green (string_of_identifier iden));
      (* Expression node *)
      print_indent (indent + 1) true;
      Printf.printf "%s\n" (red "Expression");
      print_indent (indent + 2) true;
      pretty_print_expr expr (indent + 2) true
  | PrintExpr expr ->
      Printf.printf "%s: " (red "Print");
      pretty_print_expr expr indent true;
      (* Printf.printf "\n" *)
  | FunctionCall (iden, expr) ->
      Printf.printf "%s\n" (red "Call");
      (* Identifier node *)
      print_indent (indent + 1) false;
      Printf.printf "%s: %s\n" (red "Identifier") (green (string_of_identifier iden));
      (* Expression node *)
      print_indent (indent + 1) true;
      Printf.printf "%s\n" (red "Expression");
      print_indent (indent + 2) true;
      pretty_print_expr expr (indent + 2) true;
  | Conditional (cond, then_branch, else_branch) ->
      Printf.printf "%s\n" (red "If");
      (* Condition *)
      print_indent (indent + 1) false;
      Printf.printf "%s: " (red "Condition");
      Printf.printf "\n";
      pretty_print_boolexp cond (indent + 1);
      (* Then branch *)
      print_indent (indent + 1) false;
      Printf.printf "%s\n" (red "Then");
      pretty_print_block then_branch (indent + 2) false;
      (* Else branch *)
      print_indent (indent + 1) true;
      Printf.printf "%s\n" (red "Else");
      pretty_print_block else_branch (indent + 2) true
  | Malloc var ->
    Printf.printf "%s: %s\n" (red "Malloc") (green var)
  | WhileLoop (cond, body) ->
    Printf.printf "%s\n" (red "While");
    (* Condition node *)
    print_indent (indent + 1) false;
    Printf.printf "%s: " (red "Condition");
    Printf.printf "\n";
    pretty_print_boolexp cond (indent + 1);
    (* Body node *)
    print_indent (indent + 1) true;
    Printf.printf "%s\n" (red "Body");
    pretty_print_block body (indent + 2) true
  | Atomic body ->
    Printf.printf "%s\n" (red "Atom");
    (* Body node *)
    (* print_indent (indent + 1) true; *)
    pretty_print_block body (indent + 1) true
  | ParallelBlocks (left, right) ->
    Printf.printf "%s\n" (red "Parallel");
    (* Left branch *)
    print_indent (indent + 1) false;
    Printf.printf "%s\n" (red "Left");
    pretty_print_block left (indent + 2) false;
    (* Right branch *)
    print_indent (indent + 1) true;
    Printf.printf "%s\n" (red "Right");
    pretty_print_block right (indent + 2) true
  | SkipCommand ->
    Printf.printf "%s\n" (red "Skip")

and pretty_print_expr expr indent is_last =
  match expr with
  | Integer n ->
      Printf.printf "%s%d\n" (green "") n
  | Identifier iden ->
      Printf.printf "%s\n" (green (string_of_identifier iden))
  | ArithmeticExpr (e1, op, e2) ->
      Printf.printf "%s\n" (red "Arithmetic");
      (* Left expression *)
      print_indent (indent + 1) false;
      Printf.printf "%s\n" (green "Left:");
      print_indent (indent + 2) true;
      pretty_print_expr e1 (indent + 2) true;
      (* Operator *)
      print_indent (indent + 1) false;
      Printf.printf "%s%s\n" (green "Arithm Operator: ") (green (string_of_arithOp op));
      (* Right expression *)
      print_indent (indent + 1) true;
      Printf.printf "%s\n" (green "Right:");
      print_indent (indent + 2) true;
      pretty_print_expr e2 (indent + 2) true
  | Procedure (param, body) ->
      Printf.printf "%s\n" (red "Proc");
      (* Parameter *)
      print_indent (indent + 1) false;
      Printf.printf "%s: %s\n" (red "Param") (green param);
      (* Body *)
      print_indent (indent + 1) true;
      Printf.printf "%s\n" (red "Body");
      pretty_print_block body (indent + 2) true
  | NullValue ->
      Printf.printf "%s\n" (green "null")

and pretty_print_boolexp boolexp indent =
  match boolexp with
  | Boolean b ->
      Printf.printf "%s" (green (string_of_bool b))
  | Comparison (e1, op, e2) ->
      (* Left expression *)
      print_indent (indent + 1) false;
      Printf.printf "%s\n" (green "Left:");
      print_indent (indent + 2) true;
      pretty_print_expr e1 (indent + 2) true;
      (* Operator *)
      print_indent (indent + 1) false;
      Printf.printf "%s%s\n" (green "Bool Operator: ") (green (string_of_boolOp op));
      (* Right expression *)
      print_indent (indent + 1) true;
      Printf.printf "%s\n" (green "Right:");
      print_indent (indent + 2) true;
      pretty_print_expr e2 (indent + 2) true

and string_of_arithOp op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"

and string_of_boolOp op =
  match op with
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | Eq -> "=="

and string_of_identifier iden =
  match iden with
  | Var v -> v
  | Field (v, f) -> v ^ "." ^ f

and
string_of_expr (e : Ast.expr) = match e with
  Integer n -> "Integer " ^ string_of_int n
| ArithmeticExpr (e1, ao, e2) -> "Arithmetic: (" ^ string_of_expr e1 ^ ", " ^ string_of_arithOp ao ^ ", " ^ string_of_expr e2 ^ ")"
| Identifier i -> "Identifier: (" ^ string_of_identifier i ^ ")"
| NullValue ->  "NullValue"
| Procedure (v, sl) ->  "Procedure: (" ^  v ^ ")"  

and string_of_bool b =
  if b then "true" else "false"

let rec string_of_loc l = match l with
    | Object o -> "Object " ^ string_of_int o
    | NullPointer -> "NullPointer"

and
string_of_tvalue tvalue = match tvalue with
  | Valid value -> "Valid-value: " ^ (match value with
    | FieldValue f -> "FieldValue " ^ f
    | IntegerValue n -> "IntegerValue " ^ string_of_int n
    | Location l -> "Location " ^ string_of_loc l
    | Closure (v, l, stk) -> "Closure (param: v)" )
  | RuntimeError -> "RuntimeError"

and
string_of_env env = "Environment: " ^ (string_of_list string_of_func_env env)

and
string_of_func_env (v,l) = Printf.sprintf "(%s, %s)" v (string_of_loc l)

and
string_of_state (stk, hp, addr) = "Stack: " ^ string_of_stack stk ^ "\n\nHeap Memory: " ^ string_of_heap hp ^ "\n\nAddress: " ^ string_of_int addr ^ "\n"

and
string_of_stack stk =
  if Stack.is_empty stk then
    "[]"
  else
    let my_string = (Stack.fold string_of_stack_fold "[" stk) in
    (String.sub my_string 0 ((String.length my_string) - 2)) ^ "]"

and
string_of_stack_fold s elem = s ^ (string_of_stack_elem elem) ^ ",\n"

and
string_of_stack_elem elem = match elem with
  | DeclarationFrame env -> "(DeclarationFrame: " ^ string_of_env env ^ ")"
  | CallFrame (env, other_stk) -> "(CallFrame: " ^ string_of_env env ^ ", Stack: " ^ string_of_stack other_stk ^ ")"

and
string_of_func_heap ((obj, s), tvalue) = Printf.sprintf "((%d, %s), %s)" obj s (string_of_tvalue tvalue)

and
string_of_heap hp = string_of_list string_of_func_heap hp

and
string_of_heap_location (obj, s) = Printf.sprintf "(%d, %s)" obj s