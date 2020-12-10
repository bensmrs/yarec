let compile ?(flags=[]) regex_str = Parser.start (Lexer.token flags) (Lexing.from_string regex_str)

let find_first_matching regexes str = Codegen.find_first_matching regexes (Util.explode str)

let check regex str = Codegen.check regex (Util.explode str)

let match_one regex str = Codegen.match_one regex (Util.explode str)
