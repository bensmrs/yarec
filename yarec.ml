open Yarec

let _ =
    let lexbuf = Lexing.from_channel stdin in
    ignore (Codegen.of_ast (Parser.start (Lexer.token []) lexbuf));
    print_endline "Done"
