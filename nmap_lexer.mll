{

  let props: (char * string) list ref = ref []

  let buff = Buffer.create 256

}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let prop_letter = ['a'-'z''A'-'Z''0'-'9'' ''$'':''_']
let name_letters = ['a'-'z''A'-'Z''0'-'9''-''_''.''/']

rule token regexp_parser = parse
  (* newlines *)
  | newline { Location.incr_line lexbuf; token regexp_parser lexbuf }
  (* blanks *)
  | blank + { token regexp_parser lexbuf }
  (* end of file *)
  | eof      { "EOF" }
  (* comments *)
  | '#' not_newline_char*  { token regexp_parser lexbuf }
  (* useless *)
  | "Exclude" not_newline_char*  { token regexp_parser lexbuf }
  | "Probe" not_newline_char*  { token regexp_parser lexbuf }
  | "totalwaitms" not_newline_char*  { token regexp_parser lexbuf }
  | "tcpwrappedms" not_newline_char*  { token regexp_parser lexbuf }
  | "rarity" not_newline_char*  { token regexp_parser lexbuf }
  | "ports" not_newline_char*  { token regexp_parser lexbuf }
  | "sslports" not_newline_char*  { token regexp_parser lexbuf }
  | "fallback" not_newline_char*  { token regexp_parser lexbuf }

  (* match *)
  | "match" blank+ (name_letters+ as id) blank+ 'm' (_ as c)
      {
(*        Printf.printf "reading match %s" id;*)
        let _re = regexp_parser c lexbuf in
(*        Printf.printf " re read '%s'" (Types.string_of re);*)
        let _props = rest c lexbuf in
(*        print_endline " props read";*)
        id
      }
  | "softmatch" blank+ (name_letters+ as id)   not_newline_char*        { id}

  (* illegal characters *)
  | _ as c  { Exceptions.illegal_state (Printf.sprintf "Illegal character '%c' in token: " c) (Location.curr lexbuf) }

and rest ch = parse
  | (_ as c) ((['i''s'] | "is" | "si") as m)? {
      if c = ch then
        properties lexbuf, m
      else
        Exceptions.illegal_state (Printf.sprintf "Illegal character '%c' in rest: " c) (Location.curr lexbuf)
    }

and properties = parse
  | newline { Location.incr_line lexbuf; !props }
  | eof { !props }
  | blank+ { properties lexbuf }
  | (['p''v''h''i''o''d'] as k) (_ as d)
      {
	Buffer.reset buff;
(*        Printf.printf "Delimiter %C\n" d;*)
        prop_string d lexbuf;
        let str = Buffer.contents buff in
        props := (k,str)::!props;
(*        print_endline str;*)
        properties lexbuf
      }
  | "cpe:" (_ as d)
      {
	Buffer.reset buff;
(*        Printf.printf "Delimiter %C\n" d;*)
        prop_string d lexbuf;
        let str = Buffer.contents buff in
        props := ('c',str)::!props;
(*        print_endline str;*)
        let _arg = arg lexbuf in
        properties lexbuf
      }
  (* illegal characters *)
  | _ as c  { Exceptions.illegal_state (Printf.sprintf "Illegal character in properties '%c': " c) (Location.curr lexbuf) }

and prop_string ch = parse
  | newline | eof        { Exceptions.illegal_state "Unterminated property string" (Location.curr lexbuf) }
  | _ as c
    {
(*
      Printf.printf "Reading %C and waiting %C\n" c ch;
*)
      if c = ch then
        ()
      else
        (Buffer.add_char buff c; prop_string ch lexbuf)
    }

and arg = parse
  | 'a' { true }
  | _  { LexingII.rewind lexbuf 1; false }
