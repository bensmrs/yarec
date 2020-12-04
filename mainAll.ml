open Yarec

module Lex = LexingII.Make (Parser) (Flags) (Lexer)

module ParserEof = struct
  include Parser
  let eof = EOF
end

module LexEof = LexingII.MakeEof (ParserEof)


let regexp c lexbuf = Parser.start (LexEof.lexer_end_with Lex.lexer c) lexbuf

let () =
  let file = "tests/nmap-service-probes.txt" in
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    begin
      try
        Lex.init [];
        let rec next () =
          match Nmap_lexer.token regexp lexbuf with
          | "EOF" -> print_endline "End of file"
          | _res -> (*Printf.printf "==> %s\n" res;*) next()
        in
        next()
      with
      | Parser.Error ->
        let symbol = Lexing.lexeme lexbuf in
        Printf.printf "Syntax error symbol '%s' at %s\n" symbol (Location.string_of (Location.curr lexbuf));
      | Exceptions.Illegal_state s -> Printf.printf "%s at %s\n" s.desc (Location.string_of s.loc)
    end;
    close_in (input_file)
  with Sys_error _ ->
    Printf.printf "Can't find file '%s'" file
