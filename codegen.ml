open Exceptions
open Types
open Drange

module Regex_automaton = Automaton.Make (Char_range)

let charset = Char_range.of_list [Range ('\000', '\255')]

let rec not_shorthand s = Char_range.substract charset (drange_of_shorthand s)

and drange_of_shorthand = function
  | H_space         -> Char_range.of_list [Single '\t'; Single ' '; Single '\194']
  | Non_h_space     -> not_shorthand H_space
  | V_space         -> Char_range.of_list [Single '\n'; Single '\011'; Single '\012'; Single '\r';
                                           Single '\133']
  | Non_v_space     -> not_shorthand V_space
  | White_space     -> Char_range.add (drange_of_shorthand H_space) (drange_of_shorthand V_space)
  | Non_white_space -> not_shorthand White_space
  | Digit           -> Char_range.of_list [Range ('0', '9')]
  | Non_digit       -> not_shorthand Digit
  | Word_char       -> Char_range.of_list [Range ('a', 'z'); Range ('A', 'Z'); Range ('0', '9');
                                           Single '_']
  | Non_word_char   -> not_shorthand Word_char
  | New_line        -> Char_range.of_list [Single '\n'; Single '\r']
  | Non_new_line    -> not_shorthand New_line
  | Any_char        -> charset

let atom_to_drange = function
  | Shorthand s         -> drange_of_shorthand s
  | Single c            -> Char_range.of_list [Single c]
  | Range (start, stop) -> Char_range.of_list [Range (start, stop)]

let rec of_top_expr expr =
  let rec top_expr_to_list = function
    | Expr e         -> [e]
    | Either (e, e') -> e::(top_expr_to_list e') in
  match top_expr_to_list expr with
    | [] -> raise (Unreachable_branch)
    | l  -> let (automaton, id) = Regex_automaton.add_state ~initial:true Regex_automaton.empty in
            List.fold_left (fun x y -> Regex_automaton.link_ignore ~state:[id] x (of_main_expr y))
                           automaton l

and of_main_expr = function
  | hd::tl -> List.fold_left (fun x y -> Regex_automaton.link_ignore x (of_quantified y))
                             (of_quantified hd) tl
  | []     -> raise (Invalid_argument "Cannot process an empty expression")

and of_quantified = function
  | Start_of_line                      -> failwith "Unsupported feature"
  | End_of_line                        -> failwith "Unsupported feature"
  | Quantified (atom, qual_quantifier) -> of_qual_quantifier (of_main_atom atom) qual_quantifier

and of_main_atom = function
  | Regular c              -> Regex_automaton.of_transition (Some (Char_range.of_list [Single c]))
  | Special New_line       -> failwith "Unsupported feature"
  | Special s              -> Regex_automaton.of_transition (Some (drange_of_shorthand s))
  | One_of []              -> raise Unreachable_branch
  | One_of l               -> Regex_automaton.of_transition
                                (Some (List.fold_left
                                         (fun x y -> Char_range.add x (atom_to_drange y))
                                         Char_range.empty l))
  | None_of []             -> raise Unreachable_branch
  | None_of l              -> Regex_automaton.of_transition
                                (Some (Char_range.substract charset
                                         (List.fold_left
                                           (fun x y -> Char_range.add x (atom_to_drange y))
                                           Char_range.empty l)))
  | Look_ahead _           -> failwith "Unsupported feature"
  | Negative_look_ahead _  -> failwith "Unsupported feature"
  | Look_behind _          -> failwith "Unsupported feature"
  | Negative_look_behind _ -> failwith "Unsupported feature"
  | No_capture _           -> failwith "Unsupported feature"
  | Capture _              -> failwith "Unsupported feature"
  | Back_ref _             -> failwith "Unsupported feature"

and of_qual_quantifier automaton = function
  | Greedy q     -> of_quantifier automaton q
  | Lazy _       -> failwith "Unsupported feature"
  | Possessive _ -> failwith "Unsupported feature"

and of_quantifier automaton = function
  | From_to (start, stop) when start = stop
                          -> of_quantifier automaton (Exactly start)
  | From_to (start, stop) when start > stop
                          -> raise (Invalid_argument "The max should be greater than the min")
  | From_to (0, stop) -> Regex_automaton.link_ignore
                           (Regex_automaton.repeat_bypass automaton (stop))
                           (Regex_automaton.bypass automaton)
  | From_to (start, stop) -> Regex_automaton.link_ignore
                               (Regex_automaton.link_ignore
                                  (Regex_automaton.repeat automaton start)
                                  (Regex_automaton.repeat_bypass automaton (stop-start)))
                               (Regex_automaton.bypass automaton)
  | From 0                -> Regex_automaton.loop automaton
  | From 1                -> Regex_automaton.chain automaton
  | From start            -> Regex_automaton.link_ignore
                               (Regex_automaton.repeat automaton (start-1))
                               (Regex_automaton.chain automaton)
  | Exactly n             -> Regex_automaton.repeat automaton n

let of_ast expr = Regex_automaton.check (of_top_expr expr)
