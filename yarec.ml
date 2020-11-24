open Yarec

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let _res = Parser.start (Lexer.token []) lexbuf in
    print_endline "RegEx parsed"
