let _ =
    let lexbuf = Lexing.from_channel stdin in
    let res = Parser.start (Lexer.token []) lexbuf in
    print_endline "RegEx parsed"
