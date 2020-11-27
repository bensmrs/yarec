open Yarec

let _ =
    print_string "RegEx: ";
    let regex = read_line () in
    let lexbuf = Lexing.from_string regex in
    let check = (Codegen.of_ast (Parser.start (Lexer.token []) lexbuf)) in
    print_string "Input: ";
    let input = read_line () in
    print_endline (match check (Util.explode input) with
                   | true -> "Input matched"
                   | _    -> "Input didn't match")
