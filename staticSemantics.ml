(* staticSemantics.ml *)
open Ast
open PrettyPrint

module VarSet = Set.Make(String)

let rec static_check_prog (Block p) = check_block p VarSet.empty

and check_block p v0 = match p with
  | EmptyBlock -> false
  | CommandList (c, l) -> match c with
    | DeclareVar v -> check_block l (VarSet.add v v0)
    | FunctionCall (i, e) ->
      (check_identifier i v0) || (check_expr e v0) || (check_block l v0)
    | Malloc v ->
      if VarSet.mem v v0 then
        false
      else
        true
    | Assignment (i, e) ->
      (check_identifier i v0) || (check_expr e v0) || (check_block l v0)
    | WhileLoop (b, l') ->
      (check_bool b v0) || (check_block l' v0)
    | Conditional (b, l1, l2) ->
      (check_bool b v0) || (check_block l1 v0) || (check_block l2 v0)
    | Atomic l' ->
      check_block l' v0
    | ParallelBlocks (l1, l2) ->
      (check_block l1 v0) || (check_block l2 v0)
    | SkipCommand ->
      check_block l v0
    | PrintExpr e ->
      (check_expr e v0) || (check_block l v0)

and check_expr e v0 = match e with
  | Integer _ -> false
  | ArithmeticExpr (e1, _, e2) -> (check_expr e1 v0) || (check_expr e2 v0)
  | Identifier i -> check_identifier i v0
  | NullValue -> false
  | Procedure (v, l) -> check_block l (VarSet.add v v0)

and check_identifier i v0 = match i with
  | Var v ->
    if not (VarSet.mem v v0) then
      (print_endline ("Variable " ^ v ^ " not in scope."); true)
    else
      false
  | Field (v, _) ->
    if not (VarSet.mem v v0) then
      (print_endline ("Variable " ^ v ^ " not in scope."); true)
    else
      false

and check_bool b v0 = match b with
  | Boolean _ -> false
  | Comparison (e1, _, e2) -> (check_expr e1 v0) || (check_expr e2 v0)