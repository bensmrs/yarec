let rewind lexbuf n = lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n
