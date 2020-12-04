open Yarec

module MyParser = struct
  include Parser

  let string_of = function
    | EOF -> "eof"
    | RANGE -> "range"
    | NLBRACKET -> "[^"
    | LBRACKET -> "["
    | RBRACKET -> "]"
    | PLOOKAHEAD -> "(?="
    | NLOOKAHEAD -> "(?!"
    | PLOOKBEHIND -> "(?<="
    | NLOOKBEHIND -> "(?<!"
    | NONCAPTURING -> "(?:"
    | LPARENTHESIS -> "("
    | RPARENTHESIS -> ")"
    | FROMTO(i1,i2) -> Printf.sprintf "{%i,%i}" i1 i2
    | FROM i -> Printf.sprintf "{%i,}" i
    | EXACTLY i -> Printf.sprintf "{%i}" i
    | LAZY -> "lazy"
    | POSSESSIVE -> "possesive"
    | CHAR c -> Printf.sprintf "%C" c
    | SPECIAL s -> Printf.sprintf "special %s" (Types.string_of_shorthand s)
    | STARTL -> "^"
    | ENDL -> "$"
    | START -> "^"
    | END -> "$"
    | OR -> "|"
    | BACKREF i -> Printf.sprintf "\\%i" i
end

module Lex = LexingII.Make (MyParser) (Flags) (Lexer)

let () =
  let file = "tests/pop3.re" in
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    begin
      try
        Lex.init [];
        let res = Parser.start Lex.lexer lexbuf in
        print_endline (Types.string_of res)
      with
      | Parser.Error ->
        let symbol = Lexing.lexeme lexbuf in
        Printf.printf "Syntax error symbol '%s' at %s\n" symbol (Location.string_of (Location.curr lexbuf));
      | Exceptions.Illegal_state s -> Printf.printf "%s at %s\n" s.desc (Location.string_of s.loc)
    end;
    close_in (input_file)
  with Sys_error _ ->
    Printf.printf "Can't find file '%s'" file
