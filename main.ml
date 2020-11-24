open Yarec

let () =
  let file = "test.re" in
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    begin
      try
        let res = Parser.start (Lexer.token []) lexbuf in
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
