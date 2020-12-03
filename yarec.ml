open Yarec

let _ =
    print_string "RegEx: ";
    let regex = read_line () in
    let lexbuf = Lexing.from_string regex in
    print_string "Input: ";
    let input = read_line () in
    let match_one = (Codegen.match_one (Parser.start (Lexer.token []) lexbuf)) in
    print_endline (String.concat ", " (match_one (Util.explode input)))
