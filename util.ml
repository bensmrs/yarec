let rewind lexbuf n =
  (* TODO we should ensure that n is not too large *)
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - n; } ;
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i-1) (s.[i]::l) in
  exp (String.length s - 1) []
