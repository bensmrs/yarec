exception State_error of { desc: string; loc: Location.t }

module type LEXER = sig
  type token
  type state
  type flags
  val init: flags -> state
  val next: state -> Lexing.lexbuf -> token * state option
end

module type PARSER = sig
  type token
(*
  val string_of: token -> string
*)
end

module type FLAGS = sig
  type t
end

module Make (P: PARSER) (F: FLAGS) (L: LEXER with type token := P.token and type flags := F.t list) = struct
  let lexer_state = ref None
  let init flags = lexer_state := Some(L.init flags)
  let lexer = fun lexbuf ->
    match !lexer_state with
    | None -> raise (State_error{ desc = "lexer not initialized"; loc = Location.none})
    | Some state ->
      let token, new_state = L.next state lexbuf in
      begin
        match new_state with
        | Some s -> lexer_state := Some s
        | None -> ()
      end;
(*
      print_endline (P.string_of token);
*)
      token
end

open Lexing

module type PARSER_WITH_EOF = sig
  type token
  val eof: token
end

module MakeEof (P: PARSER_WITH_EOF) = struct
  let rec get_next_char lexbuf =
(*
    print_string "GETTING NEXT CHAR";
    Printf.printf "getting next char of %s at %i" (Bytes.to_string lexbuf.lex_buffer) lexbuf.lex_curr_pos;
*)
    if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len then
      begin
        if lexbuf.lex_eof_reached then
          ((*print_endline "END REACHED";*)
           None)
        else
          (lexbuf.refill_buff lexbuf;
(*           print_endline "REFILL";*)
           get_next_char lexbuf)
      end
    else
      let ch = Bytes.get lexbuf.lex_buffer lexbuf.lex_curr_pos in
(*      Printf.printf "GETTING NEXT CHAR %C" ch;*)
      Some (ch)

  let lexer_end_with lexer ch =
    fun lexbuf ->
      match get_next_char lexbuf with
      | None -> P.eof
      | Some c when c = ch -> P.eof
      | _ -> lexer lexbuf
end
