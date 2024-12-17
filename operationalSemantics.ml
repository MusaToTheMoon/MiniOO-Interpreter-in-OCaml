(* operationalSemantics.ml *)
open Ast
open Stack
open OperationalTypes
open PrettyPrint

let malloced_vars = ref []

let rec heap_lookup x l = match l with
  | [] -> raise Not_found
  | (k, v) :: t -> if k = x then v else heap_lookup x t

let rec heap_add (k, v) d = match d with
  | [] -> [(k, v)]
  | (k', v') :: t -> if k' = k then (k, v) :: t else (k', v') :: (heap_add (k, v) t)

let rec stack_lookup v stk =
  let new_stk = Stack.copy stk in
  stack_lookup_recurse v new_stk

and stack_lookup_recurse v stk =
  if Stack.is_empty stk then
    raise (Failure ("Variable " ^ v ^ " not found."))
  else
    let frame = Stack.pop stk in
    let curr_frame_search = match frame with
      | DeclarationFrame env -> (try heap_lookup v env with Not_found -> NullPointer)
      | CallFrame (env, _) -> (try heap_lookup v env with Not_found -> NullPointer)
    in
    match curr_frame_search with
      | NullPointer -> stack_lookup_recurse v stk
      | Object obj -> obj

let rec operational_semantics (Block p) = exec_block p (Stack.create (), [], 0)

and exec_block p (stk, hp, addr) = match p with
  | EmptyBlock -> (stk, hp, addr)
  | CommandList (c, l) -> match c with
    | DeclareVar v ->
      if v = (String.capitalize_ascii v) then
        raise (Failure (v ^ " is invalid, variable name must start with lowercase letter."))
      else
        let stk2 = Stack.copy stk in
        Stack.push (DeclarationFrame [(v, Object addr)]) stk2;
        let new_hp = heap_add ((addr, "val"), Valid (Location NullPointer)) hp in
        exec_block l (stk2, new_hp, addr + 1)
    | FunctionCall (i, e) -> (
      let tval = eval_expr (Identifier i) (stk, hp, addr) in
      match tval with
      | Valid (Closure (v, l1, stack)) ->
        let stack2 = Stack.copy stack in
        Stack.push (CallFrame ([(v, Object addr)], stack)) stack2;
        let new_hp = heap_add ((addr, "val"), (eval_expr e (stk, hp, addr))) hp in
        exec_block l (exec_commands l1 (stack2, new_hp, addr + 1))
      | _ -> raise (Failure ("Could not call function " ^ string_of_identifier i))
    )
    | Malloc v ->
      malloced_vars := v :: !malloced_vars;
      let location = stack_lookup v stk in
      let new_hp = heap_add ((location, "val"), Valid (Location (Object addr))) hp in
      exec_block l (stk, new_hp, addr + 1)
    | Assignment (i, e) -> (
      let value = eval_expr e (stk, hp, addr) in
      match value with
      | RuntimeError -> raise (Failure ("Error evaluating expression " ^ string_of_expr e))
      | Valid val2 -> (
        match i with
        | Var v ->
          let location = stack_lookup v stk in
          let new_hp = heap_add ((location, "val"), value) hp in
          exec_block l (stk, new_hp, addr)
        | Field (v, f) ->
          if not (List.mem v !malloced_vars) then
            raise (Failure ("Variable " ^ v ^ " has not been malloced."))
          else
            
            let location = stack_lookup v stk in
            let new_hp = heap_add ((location, f), value) hp in
            exec_block l (stk, new_hp, addr)          
      )
    )
    | WhileLoop (b, l1) -> (
      if eval_bool b (stk, hp, addr) then
        let new_state = exec_block l1 (stk, hp, addr) in
        exec_block (CommandList (c, l)) new_state
      else
        exec_block l (stk, hp, addr)
    )
    | Conditional (b, l1, l2) -> (
      if eval_bool b (stk, hp, addr) then
        let new_state = exec_block l1 (stk, hp, addr) in
        exec_block l new_state
      else
        let new_state = exec_block l2 (stk, hp, addr) in
        exec_block l new_state
    )
    | Atomic l1 -> exec_block l (exec_block l1 (stk, hp, addr))
    | ParallelBlocks (l1, l2) ->
      if Random.bool () then
        exec_block l (exec_block l2 (exec_block l1 (stk, hp, addr)))
      else
        exec_block l (exec_block l1 (exec_block l2 (stk, hp, addr)))
      | SkipCommand -> exec_block l (stk, hp, addr)
    | PrintExpr e -> (
      match (eval_expr e (stk, hp, addr)) with
      | Valid value -> ( 
        match value with 
        | FieldValue f -> Printf.printf "%s\n" ("Field " ^ f)
        | IntegerValue n -> Printf.printf "%s\n" ("Int " ^ string_of_int n)
        | Location l -> Printf.printf "%s\n" ("Location: " ^ string_of_loc l)
        | Closure (v, l, stk) -> Printf.printf "%s\n" ("Closure: (" ^ v ^ ",");
        pretty_print_block l 0 true; Printf.printf "%s\n" (string_of_stack stk ^ ")")
        ); 
        exec_block l (stk, hp, addr)
      | RuntimeError -> Printf.printf "%s\n" "Error";
      exec_block l (stk, hp, addr)
    )

and exec_commands p (stk, hp, addr) =
  let (new_stk, new_hp, new_addr) = exec_block p (stk, hp, addr) in
  if Stack.is_empty new_stk then
    (new_stk, new_hp, new_addr)
  else
    let head = Stack.top new_stk in
    match head with
    | DeclarationFrame env -> Stack.pop new_stk; (new_stk, new_hp, new_addr)
    | CallFrame (env, stk2) -> (stk2, new_hp, new_addr)

(* Evaluate expressions *)
and eval_expr e (stk, hp, addr) = match e with
  | Integer n -> Valid (IntegerValue n)
  | ArithmeticExpr (e1, ao, e2) ->
    let v1 = eval_expr e1 (stk, hp, addr) in
    let v2 = eval_expr e2 (stk, hp, addr) in
    (match (v1, v2) with
    | (Valid (IntegerValue n1), Valid (IntegerValue n2)) -> (
      match ao with
      | Plus -> Valid (IntegerValue (n1 + n2))
      | Minus -> Valid (IntegerValue (n1 - n2))
      | Times -> Valid (IntegerValue (n1 * n2))
      | Div -> Valid (IntegerValue (n1 / n2))
    )
    | _ -> raise (Failure ("Arithmetic operation failed: both operands must be integers. Found: " ^ string_of_expr e1 ^ " and " ^ string_of_expr e2))
    )
  | Identifier i -> (
    match i with
    | Var v -> (
      try heap_lookup (stack_lookup v stk, "val") hp
      with Not_found -> raise (Failure ("Location " ^ (string_of_heap_location (stack_lookup v stk, "val")) ^ " not found."))
    )
    | Field (v, f) -> (
      try heap_lookup ((eval_field v (Var f) (stk, hp, addr)), f) hp
      with Not_found -> raise (Failure ("Location " ^ (string_of_heap_location (eval_field v (Var f) (stk, hp, addr), f)) ^ " not found."))
    )
  )
  | NullValue -> Valid (Location NullPointer)
  | Procedure (v, l) -> Valid (Closure (v, l, stk))

(* Evaluate fields *)
and eval_field v i (stk, hp, addr) = match i with
  | Var f ->
    if f <> (String.capitalize_ascii f) then
      raise (Failure (f ^ " is invalid, fields must start with uppercase letter."))
    else
      stack_lookup v stk
  | _ -> raise (Failure "Invalid field identifier.")

(* Evaluate boolean expressions *)
and eval_bool b (stk, hp, addr) = match b with
  | Boolean bool_val -> bool_val
  | Comparison (e1, bo, e2) -> (
    let v1 = eval_expr e1 (stk, hp, addr) in
    let v2 = eval_expr e2 (stk, hp, addr) in
    match (v1, v2) with
    | (Valid (IntegerValue n1), Valid (IntegerValue n2)) -> (
      match bo with
      | Lt -> n1 < n2
      | Gt -> n1 > n2
      | Leq -> n1 <= n2
      | Geq -> n1 >= n2
      | Eq -> n1 = n2
    )
    | (Valid (Location l1), Valid (Location l2)) -> (
      match bo with
      | Eq -> l1 = l2
      | _ -> raise (Failure ("Invalid operation: the boolean operator " ^ string_of_boolOp bo ^ " cannot be applied to locations."))
    )
    | (Valid (Closure _), Valid (Closure _)) -> (
      match bo with
      | Eq -> false 
      | _ -> raise (Failure ("Invalid operation: the boolean operator " ^ string_of_boolOp bo ^ " cannot be applied to functions."))
    )
    | _ -> raise (Failure ("Type mismatch: incompatible types in boolean expression. Found: " ^ string_of_expr e1 ^ " and " ^ string_of_expr e2))
  )
