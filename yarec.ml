open Yarec

let find_first_matching regexes str =
    let asts = List.map (fun r -> Parser.start (Lexer.token []) (Lexing.from_string r)) regexes in
    Codegen.find_first_matching asts (Util.explode str)

let match_one regex ?(flags=[]) str = (Codegen.match_one (Parser.start (Lexer.token flags)
                                                                       (Lexing.from_string regex)))
                                      (Util.explode str)

let _ =
  print_string (String.concat ", " (match_one "((ab)*)ab" ~flags:[`CASELESS] "Ababababab"));
  print_newline ();
  print_string (String.concat ", " (match_one "((ab)+?)ab" ~flags:[`CASELESS] "Ababababab"));
  print_newline ();
  print_string (String.concat ", " (match_one "[Z-b]" ~flags:[`CASELESS] "A"));
  print_newline ();
  print_int (find_first_matching ["^foo(bar)"; "^ding[do]ng?$"; "testtest" ] "dingon");
  print_newline ()
