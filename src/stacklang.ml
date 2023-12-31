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
                         start: int;
                         cursor: int;
                         steps: Automaton.step list * Automaton.step list }
  type runtime_state = generic_state * data
  type transition_item = Char_range.t Automaton.transition
  val empty : t
  val add_state : ?f:(runtime_state -> runtime_state) -> ?initial:bool -> ?final:bool -> t -> t * int
  val add_transition : t -> int -> int -> transition_item -> t
  val single :  ?f:(runtime_state -> runtime_state) -> unit -> t
  val of_transition :  ?f:(runtime_state -> runtime_state) -> transition_item -> t
  val compose_states : t -> int list -> (runtime_state -> runtime_state) -> t
  val link : ?state:int list -> ?state':int list -> ?keep_final:bool -> t -> t ->
             t * (int, int) Hashtbl.t
  val link_ignore : ?state:int list -> ?state':int list -> ?keep_final:bool -> t -> t -> t
  val repeat : t -> ?initial:int list -> ?final:int list -> int -> t
  val from_to : t -> ?initial:int list -> ?final:int list -> int -> int -> t
  val chain : ?initial:int list -> ?final:int list -> t -> t
  val bypass : ?initial:int list -> t -> t
  val loop : ?initial:int list -> ?final:int list -> t -> t
  val check_with : t -> runtime_state list -> runtime_state option
  val check : t -> buffer -> bool
  val reverse : t -> t
  val match_one : t -> buffer -> runtime_state option
  val match_first : t -> buffer -> runtime_state option
  val with_greed : t -> int -> Automaton.greed -> t
  val final : t -> int list
  val order : t -> int
  val size : t -> int
end = Automaton.Make (Char_range) (Regex_bistack)

open Regex_bistack

type rstate = Regex_automaton.runtime_state

type instruction = CURSOR | EQUAL | ASSERT | BUFFERSIZE | CAPTURE | CHECK | SAVE | RESTORE | NOT
                 | RECALL | CONSUME | REVERSE | RESTORECUR | RESTORESTATE
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
      | CHECK, Automaton a::tl -> (match Regex_automaton.check_with
                                           a [({ gstate with id = 0 }, data)] with
                                   | Some (_, data) -> push (Bool true) (gstate, { data with stack = tl })
                                   | None           -> push (Bool false) (gstate, { data with stack = tl }))
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
      | RESTORECUR, Rstate (gs, _)::tl
                               -> ({ gstate with cursor = gs.cursor }, { data with stack = tl })
      | RESTORECUR, _          -> failwith "RESTORECUR requires an Rstate"
      | RESTORESTATE, Rstate (gs, _)::tl
                               -> (gs, { data with stack = tl })
      | RESTORESTATE, _        -> failwith "RESTORESTATE requires an Rstate"
      | NOT, Bool b::tl        -> push (Bool (not b)) (gstate, { data with stack = tl })
      | NOT, _                 -> failwith "NOT requires a Bool"
      | CAPTURE, Int i::Int e::Int b::tl
                               -> let s = Util.string_of_chars (Util.slice gstate.buffer b e) in
                                  (gstate, { stack = tl;
                                             captures = Util.set_nth_safe data.captures i s "" })
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
