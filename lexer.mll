{
  let verbatim = ref false
  let range_state = ref 3
  let init_verbatim () = verbatim := true; range_state := 3
  let char_step () = range_state :=
    begin
      match !range_state with
      | 2 -> 0
      | _ -> 1
    end
  let special_step () = range_state :=
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
  | _   { rewind lexbuf 1; if !verbatim then verbatim flags lexbuf else default lexbuf }

and default flags = parse
  | '[' '^'                                   { init_verbatim (); NLBRACKET }
  | '['                                       { init_verbatim (); LBRACKET }
  | '(' '?' '='                               { PLOOKAHEAD }
  | '(' '?' '!'                               { NLOOKAHEAD }
  | '(' '?' '<' '='                           { PLOOKBEHIND }
  | '(' '?' '<' '!'                           { NLOOKBEHIND }
  | '(' '?' ':'                               { NONCAPTURING }
  | '(' '?' '#'                               { comment (Location.curr lexbuf) lexbuf; default lexbuf }
  | '('                                       { LPARENTHESIS }
  | ')'                                       { RPARENTHESIS }
  | '{' (digit+ as s1) ',' (digit+ as s2) '}' { FROMTO (int_of_string s1, int_of_string s2) }
  | '{' (digit+ as s) ',' '}'                 { FROM (int_of_string s) }
  | '{' (digit+ as s) '}'                     { EXACTLY (int_of_string s) }
  | '?' '?'                                   { LAZYMAY }
  | '?'                                       { FROMTO (0,1) }
  | '+' '?'                                   { LAZYMUST }
  | '+'                                       { FROM 1 }
  | '*' '?'                                   { LAZYANY }
  | '*'                                       { FROM 0 }
  | '\\'                                      { special lexbuf }
  | '.'                                       { if Flags.dotall flags then ANYCHAR else NONNEWLINE }
  | _ as c                                    { CHAR c }

and verbatim = parse
  | '-' ']' { rewind lexbuf 1; CHAR '-' }
  | ']'     { if verbatim_has_started () then (verbatim := false; RBRACKET) else CHAR ']' }
  | '\\'    { special lexbuf }
  | '-'     { if range_is_char () then CHAR '-' else RANGE }
  | _ as c  { char_step (); CHAR c }

and special = parse
  (*           { Special rule } { Character rule } *)
  | 'h'        { special_step (); HSPACE }
  | 'H'        { special_step (); NONHSPACE }
  | 'v'        { special_step (); VSPACE }
  | 'V'        { special_step (); NONVSPACE }
  | 's'        { special_step (); WHITESPACE }
  | 'S'        { special_step (); NONWHITESPACE }
  | 'd'        { special_step (); DIGIT }
  | 'D'        { special_step (); NONDIGIT }
  | 'w'        { special_step (); WORDCHAR }
  | 'W'        { special_step (); NONWORDCHAR }
  | 'X' | 'C'  { special_step (); ANYCHAR }
  | 'R'        { special_step (); NEWLINE }
  | 'x' hexdigit hexdigit as s  { char_step (); CHAR (Char.chr (Scanf.sscanf s "%x" (fun x -> x))) }
  | digit digit digit as s      { char_step (); CHAR (Char.chr (int_of_string s)) }
  | '0'                         { char_step (); CHAR (Char.chr 0) }
  | digit as c { special_step (); BACKREF int_of_string (String.make 1 c) }
  | 'a'                         { char_step (); CHAR (Char.chr 7) }
  | 'b'                         { char_step (); CHAR '\b' }
  | 'f'                         { char_step (); CHAR (Char.chr 12) }
  | 'n'                         { char_step (); CHAR '\n' }
  | 'r'                         { char_step (); CHAR '\r' }
  | 't'                         { char_step (); CHAR '\t' }
  | _ as c                      { char_step (); CHAR c }

and comment start_loc = parse
  | ")" { () }
  | eof { raise (Illegal_state { desc = "Unmatched comment start"; loc = start_loc }) }
  | _   { comment start_loc lexbuf }
