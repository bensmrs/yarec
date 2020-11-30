open Drange

module rec Regex_bistack : sig
  type value = Int of int | Char of char | Bool of bool | Automaton of Regex_automaton.t
             | String of string | Rstate of Regex_automaton.runtime_state
  type t = { stack: value list;
             captures: string list }
  val empty : t
end = struct
  type value = Int of int | Char of char | Bool of bool | Automaton of Regex_automaton.t
             | String of string | Rstate of Regex_automaton.runtime_state
  type t = { stack: value list;
             captures: string list }
  let empty = { stack = [];
                captures = [] }
end
and Regex_automaton : sig
  type t
  type data = Regex_bistack.t
  type buffer = char list
  type generic_state = { id: int;
                         buffer: buffer;
                         cursor: int }
  type runtime_state = generic_state * data
  type transition_item = Char_range.t option
  val empty : t
  val add_state : ?f:(runtime_state -> runtime_state) -> ?initial:bool -> ?final:bool -> t -> t * int
  val single :  ?f:(runtime_state -> runtime_state) -> unit -> t
  val of_transition :  ?f:(runtime_state -> runtime_state) -> transition_item -> t
  val link : ?state:int list -> ?state':int list -> ?keep_final:bool -> t -> t ->
             t * (int, int) Hashtbl.t
  val link_ignore : ?state:int list -> ?state':int list -> ?keep_final:bool -> t -> t -> t
  val repeat : t -> ?initial:int list -> ?final:int list -> int -> t
  val repeat_bypass : t -> ?initial:int list -> ?final:int list -> ?f:(runtime_state ->
                      runtime_state) -> int -> t
  val chain : ?initial:int list -> ?final:int list -> t -> t
  val bypass : t -> t
  val loop : ?initial:int list -> ?final:int list -> t -> t
  val check_with : t -> runtime_state list * runtime_state list -> bool
  val check : t -> buffer -> bool
  val reverse : t -> t
end = Automaton.Make (Char_range) (Regex_bistack)

open Regex_bistack

type rstate = Regex_automaton.runtime_state

type instruction = CURSOR | EQUAL | ASSERT | BUFFERSIZE | CAPTURE | CHECK | SAVE | RESTORE | NOT
                 | RECALL | CONSUME | REVERSE
                 | INT of int | CHAR of char | AUTOMATON of Regex_automaton.t

let to_fun instructions =
  let f (rstate : rstate) =
    let push v (gstate, data) = (gstate, { stack = v::data.stack; captures = data.captures }) in
    let consume (gstate:Regex_automaton.generic_state) c =
      if List.nth gstate.buffer gstate.cursor != c
      then failwith "CONSUME: Nothing to consume"
      else { gstate with cursor = gstate.cursor+1 } in
    let process instruction ((gstate, data) : rstate) = match instruction, data.stack with
      | INT i, _               -> push (Int i) (gstate, data)
      | CHAR c, _              -> push (Char c) (gstate, data)
      | AUTOMATON a, _         -> push (Automaton a) (gstate, data)
      | CURSOR, _              -> push (Int gstate.cursor) (gstate, data)
      | CHECK, Automaton a::tl -> push (Bool (Regex_automaton.check_with
                                                a ([({ gstate with id = 0 }, data)], [])))
                                       (gstate, { data with stack = tl })
      | CHECK, _               -> failwith "CHECK requires an Automaton"
      | EQUAL, hd::hd'::tl     -> push (Bool (hd = hd')) (gstate, { data with stack = tl })
      | EQUAL, _               -> failwith "Not enough to consume"
      | ASSERT, Bool true::tl  -> (gstate, { data with stack = tl })
      | ASSERT, Bool false::_  -> failwith "Assertion failed"
      | ASSERT, _::_           -> failwith "ASSERT: TypeError"
      | ASSERT, _              -> failwith "Not enough to consume"
      | BUFFERSIZE, _          -> push (Int (List.length gstate.buffer))
                                            (gstate, data)
      | SAVE, _                -> push (Rstate (gstate, data)) (gstate, data)
      | RESTORE, Rstate rs::_  -> rs
      | RESTORE, _             -> failwith "RESTORE requires an Rstate"
      | NOT, Bool b::tl        -> push (Bool (not b)) (gstate, { data with stack = tl })
      | NOT, _                 -> failwith "NOT requires a Bool"
      | CAPTURE, Int i::Int e::Int b::tl
                               -> let s = Util.string_of_chars (Util.slice gstate.buffer b e) in
                                  (gstate, { stack = tl;
                                             captures = Util.set_nth data.captures i s })
      | CAPTURE, _             -> failwith "CAPTURE requires three Int"
      | RECALL, Int i::tl      -> (gstate, { data with stack = (String (List.nth
                                                                          data.captures i))::tl })
      | RECALL, _              -> failwith "RECALL requires an Int"
      | CONSUME, Char c::tl    -> (consume gstate c, { data with stack = tl })
      | CONSUME, String s::tl  -> (List.fold_left consume gstate (Util.explode s),
                                   { data with stack = tl })
      | CONSUME, _             -> failwith "CONSUME requires a Char or a String"
      | REVERSE, _             -> ({ gstate with buffer = List.rev gstate.buffer;
                                                 cursor = List.length
                                                            gstate.buffer - gstate.cursor},
                                   data) in
    List.fold_left (fun s i -> process i s) rstate instructions in
  f
