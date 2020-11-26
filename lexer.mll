{
  open Exceptions
  open Parser

  type lexer_state = Default | Quantifying | Verbatim
  let global_state = ref Default
  let range_state = ref 3
  let init_verbatim () = global_state := Verbatim; range_state := 3
  let char_step () = range_state :=
    begin
      match !range_state with
      | 2 -> 0
      | _ -> 1
    end
  let special_step lexbuf = range_state :=
    begin
      match !range_state with
      | 2 -> raise (Illegal_state { desc = "A range cannot be created with a shorthand escape sequence"; loc = Location.curr lexbuf })
      | _ -> 0
    end
  let dash_step () = range_state := (!range_state + 1) mod 3
  let range_is_char () = !range_state != 2
  let verbatim_has_started () = !range_state != 3
}

let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f''A'-'F']

rule token flags = parse
  | eof { EOF }
  | _   { Util.rewind lexbuf 1; if !global_state = Verbatim then verbatim flags lexbuf else default flags lexbuf }

and default flags = parse
  | '[' '^'                                   { init_verbatim (); NLBRACKET }
  | '['                                       { init_verbatim (); LBRACKET }
  | '(' '?' '='                               { PLOOKAHEAD }
  | '(' '?' '!'                               { NLOOKAHEAD }
  | '(' '?' '<' '='                           { PLOOKBEHIND }
  | '(' '?' '<' '!'                           { NLOOKBEHIND }
  | '(' '?' ':'                               { NONCAPTURING }
  | '(' '?' '#'                               { comment (Location.curr lexbuf) lexbuf; default flags lexbuf }
  | '('                                       { LPARENTHESIS }
  | ')'                                       { RPARENTHESIS }
  | '{' (digit+ as s1) ',' (digit+ as s2) '}' { global_state := Quantifying; FROMTO (int_of_string s1, int_of_string s2) }
  | '{' (digit+ as s) ',' '}'                 { global_state := Quantifying; FROM (int_of_string s) }
  | '{' (digit+ as s) '}'                     { global_state := Quantifying; EXACTLY (int_of_string s) }
  | '?'                                       { match !global_state with Quantifying -> (global_state := Default; LAZY) | _ -> (global_state := Quantifying; FROMTO (0,1)) }
  | '+'                                       { match !global_state with Quantifying -> (global_state := Default; POSSESSIVE) | _ -> (global_state := Quantifying; FROM 1) }
  | '*'                                       { global_state := Quantifying; FROM 0 }
  | '\\'                                      { special lexbuf }
  | '.'                                       { if Flags.has `DOTALL flags then ANYCHAR else NONNEWLINE }
  | '^'                                       { STARTL }
  | '$'                                       { ENDL }
  | '|'                                       { OR }
  | _ as c                                    { CHAR c }

and verbatim flags = parse
  | '-' ']' { Util.rewind lexbuf 1; (char_step (); CHAR '-') }
  | ']'     { if verbatim_has_started () then (global_state := Default; RBRACKET) else (char_step (); CHAR ']') }
  | '\\'    { special lexbuf }
  | '-'     { dash_step (); if range_is_char () then CHAR '-' else RANGE }
  | _ as c  { char_step (); CHAR c }

and special = parse
  (*           { Special rule }  { Character rule } *)
  | 'h'        { special_step lexbuf; HSPACE }
  | 'H'        { special_step lexbuf; NONHSPACE }
  | 'v'        { special_step lexbuf; VSPACE }
  | 'V'        { special_step lexbuf; NONVSPACE }
  | 's'        { special_step lexbuf; WHITESPACE }
  | 'S'        { special_step lexbuf; NONWHITESPACE }
  | 'd'        { special_step lexbuf; DIGIT }
  | 'D'        { special_step lexbuf; NONDIGIT }
  | 'w'        { special_step lexbuf; WORDCHAR }
  | 'W'        { special_step lexbuf; NONWORDCHAR }
  | 'X' | 'C'  { special_step lexbuf; ANYCHAR }
  | 'R'        { special_step lexbuf; NEWLINE }
  | 'x' (hexdigit hexdigit as s) { char_step (); CHAR (Char.chr (Scanf.sscanf s "%x" (fun x -> x))) }
  | digit digit digit as s       { char_step (); CHAR (Char.chr (int_of_string s)) }
  | '0'                          { char_step (); CHAR (Char.chr 0) }
  | digit as c { special_step lexbuf; BACKREF (int_of_string (String.make 1 c)) }
  | 'a'                          { char_step (); CHAR (Char.chr 7) }
  | 'b'                          { char_step (); CHAR '\b' }
  | 'f'                          { char_step (); CHAR (Char.chr 12) }
  | 'n'                          { char_step (); CHAR '\n' }
  | 'r'                          { char_step (); CHAR '\r' }
  | 't'                          { char_step (); CHAR '\t' }
  | _ as c                       { char_step (); CHAR c }

and comment start_loc = parse
  | ")" { () }
  | eof { raise (Illegal_state { desc = "Unmatched comment start"; loc = start_loc }) }
  | _   { comment start_loc lexbuf }
