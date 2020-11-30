let rewind lexbuf n =
  (* TODO we should ensure that n is not too large *)
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - n; } ;
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i-1) (s.[i]::l) in
  exp (String.length s - 1) []

let rec find_index ?(index=0) e = function
  | hd::_ when hd = e -> index
  | _::tl             -> find_index ~index:(index+1) e tl
  | []                -> raise Not_found

let string_of_chars chars = 
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let rec take l n = match l, n with
  | hd::tl, i when i > 0 -> hd::(take tl (i-1))
  | _, _                 -> []

let rec drop l n = match l, n with
  | _::tl, i when i > 0 -> drop tl (i-1)
  | _, _                -> l

let cons_to_nth l n e = (take l n) @ (e::List.nth l n)::(drop l (n+1))
let append_to_nth l n e = (take l n) @ ((List.nth l n) @ e)::(drop l (n+1))
let set_nth l n e = (take l n) @ e::(drop l (n+1))
let slice l start stop = (take (drop l start) (stop-start))

let (@@) f f' = fun x -> f (f' x)
