open Exceptions
open Types
open Drange
open Stacklang
open Regex_automaton

let charset = Char_range.of_list [Range ('\000', '\255')]

let capture_id = ref 0

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

let rec of_top_expr level expr =
  let rec top_expr_to_list = function
    | Expr e         -> [e]
    | Either (e, e') -> e::(top_expr_to_list e') in
  match top_expr_to_list expr with
    | [] -> raise (Unreachable_branch)
    | l  -> let (automaton, id) = add_state ~initial:true empty in
            let (automaton, id') = add_state automaton in
            let automaton = add_transition automaton id id' Epsilon in
            List.fold_left (fun x y -> link_ignore ~state:[id'] ~keep_final:true x
                                                   (of_main_expr level y)) automaton l

and of_main_expr level = function
  | hd::tl -> List.fold_left (fun x y -> link_ignore x (of_quantified level y))
                                                       (of_quantified level hd) tl
  | []     -> Regex_automaton.single ()

and of_quantified level = function
  | Start_of_line             -> of_top_expr (level+1)
                                   (Either ([Start_of_input],
                                            Expr [
                                              Quantified (Look_behind (Expr [
                                                Quantified (Special New_line, Greedy (Exactly 1))]),
                                                          Greedy (Exactly 1))]))
  | End_of_line               -> of_top_expr (level+1)
                                   (Either ([End_of_input],
                                            Expr [
                                              Quantified (Look_ahead (Expr [
                                                Quantified (Special New_line, Greedy (Exactly 1))]),
                                                          Greedy (Exactly 1))]))
  | Start_of_input            -> of_transition ~f:(to_fun [CURSOR; INT 0; EQUAL; ASSERT]) Epsilon
  | End_of_input              -> of_transition ~f:(to_fun [CURSOR; BUFFERSIZE; EQUAL; ASSERT])
                                               Epsilon
  | Quantified (atom, qual_q) -> of_qual_quantifier level (of_main_atom level atom) qual_q

and of_main_atom level = function
  | Regular c                 -> of_transition (Consume (Char_range.of_list [Single c]))
  | Special New_line          -> of_top_expr (level+1)
                                   (Either ([Quantified (One_of [Shorthand New_line],
                                                         Greedy (Exactly 1))],
                                            Expr [Quantified (Regular '\r', Greedy (Exactly 1));
                                                  Quantified (Regular '\n', Greedy (Exactly 1))]))
  | Special s                 -> of_transition (Consume (drange_of_shorthand s))
  | One_of []                 -> raise Unreachable_branch
  | One_of l                  -> of_transition
                                   (Consume (List.fold_left
                                               (fun x y -> Char_range.add x (atom_to_drange y))
                                               Char_range.empty l))
  | None_of []                -> raise Unreachable_branch
  | None_of l                 -> of_transition
                                   (Consume (Char_range.substract charset
                                               (List.fold_left
                                                 (fun x y -> Char_range.add x (atom_to_drange y))
                                                 Char_range.empty l)))
  | Look_ahead expr           -> link_ignore
                                   (link_ignore
                                      (of_transition ~f:(to_fun [SAVE]) Epsilon)
                                      (of_top_expr (level+1) expr))
                                   (of_transition ~f:(to_fun [RESTORECUR]) Epsilon)
  | Negative_look_ahead expr  -> of_transition ~f:(to_fun [SAVE;
                                                           AUTOMATON (of_top_expr (level+1) expr);
                                                           CHECK; NOT; ASSERT; RESTORE]) Epsilon
  | Look_behind expr          -> of_transition
                                   ~f:(to_fun [SAVE; REVERSE;
                                               AUTOMATON (reverse (of_top_expr (level+1) expr));
                                               CHECK; ASSERT; RESTORESTATE]) Epsilon
  | Negative_look_behind expr -> of_transition
                                   ~f:(to_fun [SAVE; REVERSE;
                                               AUTOMATON (reverse (of_top_expr (level+1) expr));
                                               CHECK; NOT; ASSERT; RESTORE]) Epsilon
  | No_capture expr           -> of_top_expr (level+1) expr
  | Capture expr              -> let cid = !capture_id in capture_id := cid + 1;
                                 link_ignore
                                   (link_ignore
                                      (of_transition ~f:(to_fun [CURSOR]) Epsilon)
                                      (of_top_expr (level+1) expr))
                                   (of_transition ~f:(to_fun [CURSOR; INT cid; CAPTURE]) Epsilon)
  | Back_ref id               -> of_transition ~f:(to_fun [INT (id-1); RECALL; CONSUME]) Epsilon

and of_qual_quantifier level automaton = function
  | Greedy q     -> with_greed (of_quantifier automaton q) level Automaton.Greedy
  | Lazy q       -> with_greed (of_quantifier automaton q) level Automaton.Lazy
  | Possessive _ -> failwith "Unsupported feature: +"

and of_quantifier automaton = function
  | From_to (start, stop) when start = stop
                          -> of_quantifier automaton (Exactly start)
  | From_to (start, stop) when start > stop
                          -> raise (Invalid_argument "The max should be greater than the min")
  | From_to (start, stop) -> link_ignore
                               (link_ignore
                                  (repeat automaton start)
                                  (repeat_bypass automaton (stop-start-1)))
                               (bypass automaton)
  | From 0                -> loop automaton
  | From start            -> link_ignore
                               (repeat automaton (start-1))
                               (chain automaton)
  | Exactly n             -> repeat automaton n

let check expr = capture_id := 0; check (of_top_expr 0 expr)
let match_one expr buffer =
  capture_id := 0;
  let (rstate, data) = Option.get (match_one (of_top_expr 0 expr) buffer) in
  Util.string_of_chars (Util.slice rstate.buffer rstate.start rstate.cursor)::data.captures
