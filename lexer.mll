{
  open Exceptions
  open Parser

  type lexer_token =
    | Ext of token
    | Plus
    | Quest
    | Dot
    | SpecialChar of char
    | VerbChar of char
    | CloseBra
    | Dash
    | Start
    | End
    | Comment


  let special_char = function
    | 'h' -> SPECIAL H_space
    | 'H' -> SPECIAL Non_h_space
    | 'v' -> SPECIAL V_space
    | 'V' -> SPECIAL Non_v_space
    | 's' -> SPECIAL White_space
    | 'S' -> SPECIAL Non_white_space
    | 'd' -> SPECIAL Digit
    | 'D' -> SPECIAL Non_digit
    | 'w' -> SPECIAL Word_char
    | 'W' -> SPECIAL Non_word_char
    | 'X' | 'C' -> SPECIAL Any_char
    | 'R' -> SPECIAL New_line
    | c -> failwith(Printf.sprintf "special_char called with '%c'" c)

}

let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f''A'-'F']
let special_chars = ['h''H''v''V''s''S''d''D''w''W''X''C''R']

rule default = parse
  | '[' '^'                                   { Ext NLBRACKET }
  | '['                                       { Ext LBRACKET }
  | '(' '?' '='                               { Ext PLOOKAHEAD }
  | '(' '?' '!'                               { Ext NLOOKAHEAD }
  | '(' '?' '<' '='                           { Ext PLOOKBEHIND }
  | '(' '?' '<' '!'                           { Ext NLOOKBEHIND }
  | '(' '?' ':'                               { Ext NONCAPTURING }
  | '(' '?' '#'                               { comment (Location.curr lexbuf) lexbuf; Comment }
  | '('                                       { Ext LPARENTHESIS }
  | ')'                                       { Ext RPARENTHESIS }
  | '{' (digit+ as s1) ',' (digit+ as s2) '}' { Ext(FROMTO(int_of_string s1, int_of_string s2)) }
  | '{' (digit+ as s) ',' '}'                 { Ext(FROM(int_of_string s)) }
  | '{' (digit+ as s) '}'                     { Ext(EXACTLY(int_of_string s)) }
  | '?'                                       { Quest }
  | '+'                                       { Plus }
  | '*'                                       { Ext(FROM 0) }
  | '\\'                                      { special lexbuf }
  | '.'                                       { Dot }
  | '^'                                       { Start }
  | '$'                                       { End }
  | '|'                                       { Ext OR }
  | eof                                       { Ext EOF }
  | _ as c                                    { Ext(CHAR c) }

and special = parse
  (*           { SPECIAL rule } { Character rule } *)
  | special_chars as c              { Ext (special_char c) }
  | 'x' (hexdigit hexdigit as s)    { SpecialChar (Char.chr (Scanf.sscanf s "%x" (fun x -> x))) }
  | digit digit digit as s          { SpecialChar (Char.chr (int_of_string s)) }
  | '0'                             { SpecialChar (Char.chr 0) }
  | digit as c                      { Ext (BACKREF (int_of_string (String.make 1 c))) }
  | 'a'                             { SpecialChar (Char.chr 7) }
  | 'b'                             { SpecialChar '\b' }
  | 'f'                             { SpecialChar (Char.chr 12) }
  | 'n'                             { SpecialChar '\n' }
  | 'r'                             { SpecialChar '\r' }
  | 't'                             { SpecialChar '\t' }
  (* Non alphanumeric symbols *)
  | ([^'0'-'9''A'-'Z''a'-'z'] as c) { SpecialChar c }
  | _ as c                          { illegal_state (Printf.sprintf "Unknown character '%c'" c) (Location.curr lexbuf) }

and comment start_loc = parse
  | ")" { () }
  | eof { illegal_state "Unmatched comment start" start_loc }
  | _   { comment start_loc lexbuf }

and verbatim = parse
  | '-' ']' { LexingII.rewind lexbuf 1; VerbChar '-' }
  | ']'     { CloseBra }
  | '\\'    { special lexbuf }
  | '-'     { Dash }
  | eof     { Ext EOF }
  | _ as c  { VerbChar c }

{
  type state_kind = Default | Quantifying | Verbatim
  type range_kind = Z | I | II | III
  type state = {
    kind: state_kind;
    flags: Flags.t list;
    lexer: Lexing.lexbuf -> lexer_token;
    range: range_kind;
  }

  let init flags = { kind = Default; flags = flags; lexer = default; range = III }

  let rec next state lexbuf =
    match state.lexer lexbuf with
    (* character range starts *)
    | Ext((NLBRACKET | LBRACKET) as t) ->
      t, Some { state with kind = Verbatim; lexer = verbatim ; range = III}
    (* quantifiers *)
    | Ext((FROMTO _ | FROM _ | EXACTLY _) as t) ->
      t, Some { state with kind = Quantifying }
    | Quest when state.kind = Quantifying ->
      LAZY, Some { state with kind = Default }
    | Quest ->
      FROMTO (0,1), Some { state with kind = Quantifying }
    | Plus  when state.kind = Quantifying ->
      POSSESSIVE, Some { state with kind = Default }
    | Plus ->
      FROM 1, Some { state with kind = Quantifying }
    (* Special characters *)
    | Ext (SPECIAL _ | BACKREF _) when state.range = II ->
      illegal_state "A range cannot be created with a shorthand escape sequence" (Location.curr lexbuf)
    | Ext((SPECIAL _ | BACKREF _) as t) when state.kind = Quantifying ->
      t, Some { state with kind = Default;range = Z }
    | Ext((SPECIAL _ | BACKREF _) as t) ->
      t, Some { state with range = Z }
    | SpecialChar c ->
      let new_range = match state.range with II -> Z | _ -> I in
      let new_state = if state.kind <> Verbatim then Default else Verbatim in
      CHAR c, Some { state with kind = new_state; range = new_range }
    (* Other character not in verbatim *)
    | Ext(CHAR _ as t) when state.kind <> Verbatim ->
      t, Some { state with kind = Default }
    | Start ->
      (if Flags.has `MULTILINE state.flags then STARTL else START), Some { state with kind = Default }
    | End ->
      (if Flags.has `MULTILINE state.flags then ENDL else END), Some { state with kind = Default }
    | Ext OR ->
      OR, Some { state with kind = Default }
    | Dot ->
      (if Flags.has `DOTALL state.flags then SPECIAL Any_char else SPECIAL Non_new_line), Some { state with kind = Default }
    | Ext((PLOOKAHEAD | NLOOKAHEAD | PLOOKBEHIND | NLOOKBEHIND | NONCAPTURING | LPARENTHESIS | RPARENTHESIS) as t) ->
      t, Some { state with kind = Default }
    | Comment ->
      next { state with kind = Default } lexbuf
    (* Verbatim specific characters *)
    | CloseBra when state.range != III ->
      RBRACKET, Some { state with kind = Default; lexer = default }
    | CloseBra ->
      CHAR ']', Some { state with range = I }
    | Dash when state.range = I ->
      RANGE, Some { state with range = II }
    | Dash when state.range = II ->
      CHAR '-', Some { state with range = Z }
    | Dash ->
      CHAR '-', Some { state with range = I }
    | VerbChar c when state.range = II ->
      CHAR c, Some { state with range = Z }
    | VerbChar c ->
      CHAR c, Some { state with range = I }
    (* END *)
    | Ext EOF ->
      EOF, None
    (* ERRORS *)
    | Ext(RBRACKET | RANGE | LAZY | POSSESSIVE | START | END | STARTL | ENDL | CHAR _) -> (* CHAR in Verbatim *)
      failwith(Printf.sprintf "should not appear in internal lexers")
}

