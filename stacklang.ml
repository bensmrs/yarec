open Drange

type instruction = INDEX | INT of int | CHAR of char | EQUAL | ASSERT | BUFFERSIZE | MARK | CAPTURE
type value = Int of int | Char of char | Bool of bool | Mark

module Regex_bistack = struct
  type t = { stack: value list;
             captures: string list }
  let empty = { stack = [];
                captures = [] }
end

module Regex_automaton = Automaton.Make (Char_range) (Regex_bistack)

let to_fun instructions =
  let f ((rstate, data) : Regex_automaton.runtime_state) =
    let rec consume acc id (data : Regex_bistack.t) = function
      | Mark::tl   -> (tl, Util.set_nth data.captures id (Util.string_of_chars acc))
      | Char c::tl -> consume (c::acc) id data tl
      | _::_       -> failwith "CAPTURE: TypeError (non-Char encountered before Mark)"
      | []         -> failwith "CAPTURE: Mark not found" in
    let process instruction (data : Regex_bistack.t) = match instruction, data.stack with
      | INDEX, s              -> (Int (rstate.id)::s, data.captures)
      | INT i, s              -> (Int i::s, data.captures)
      | CHAR c, s             -> (Char c::s, data.captures)
      | EQUAL, hd::hd'::tl    -> (Bool (hd = hd')::tl, data.captures)
      | EQUAL, _              -> failwith "Not enough to consume"
      | ASSERT, Bool true::tl -> (tl, data.captures)
      | ASSERT, Bool false::_ -> failwith "Assertion failed"
      | ASSERT, _::_          -> failwith "ASSERT: TypeError"
      | ASSERT, _             -> failwith "Not enough to consume"
      | BUFFERSIZE, s         -> (Int (List.length rstate.buffer)::s, data.captures)
      | MARK, s               -> (Mark::s, data.captures)
      | CAPTURE, Int i::tl    -> consume [] i data tl
      | CAPTURE, _            -> failwith "CAPTURE requires an Int" in
    (rstate, List.fold_left (fun d i -> let (stack, captures) = process i d in { stack; captures })
                            data instructions) in
  f
