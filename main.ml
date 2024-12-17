(* main.ml *)
open Ast
open PrettyPrint
open StaticSemantics
open OperationalSemantics

let () =
  Random.self_init ();

  try
    let lexbuf = Lexing.from_channel stdin in
    (* Parse the input *)
    let abstractSyntaxTree =
      try
        Parser.prog Lexer.token lexbuf
      with
      | Parser.Error ->
          let pos = Lexing.lexeme_start_p lexbuf in
          failwith (Printf.sprintf "Parser error at line %d, column %d." pos.pos_lnum (pos.pos_cnum - pos.pos_bol))
      | Lexer.Failure msg ->
          failwith ("Lexer error: " ^ msg)
    in

    (* Perform static checks *)
    if StaticSemantics.static_check_prog abstractSyntaxTree then
      failwith "Static check failed."
    else
      (
        pretty_print_prog abstractSyntaxTree;
        print_endline "\nOutput:";
        let final_state = operational_semantics abstractSyntaxTree in
        print_endline "\nFinal State of the Program:";
        print_endline (string_of_state final_state)
      )
  with
  | Failure msg -> prerr_endline ("Error: " ^ msg)
  | Sys_error msg -> prerr_endline ("System error: " ^ msg)
