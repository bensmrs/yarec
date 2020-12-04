open Exceptions
open Types
open Drange
open Stacklang
open Regex_automaton

let charset = Char_range.of_list [Range ('\000', '\255')]

let capture_id = ref 0

let is_letter c =
  let cc = Char.code c in
  65 <= cc && cc <= 90 || 97 <= cc && cc <= 122 || 192 <= cc && cc <= 214 ||
  216 <= cc && cc <= 246 || 248 <= cc

let get_subrange c =
  match Char.code c with
  | cc when cc < 65   -> 0
  | cc when cc <= 90  -> 1 (* uppercase letters *)
  | cc when cc < 97   -> 2
  | cc when cc <= 122 -> 3 (* lowercase letters *)
  | cc when cc < 192  -> 4
  | cc when cc <= 214 -> 5 (* uppercase accented *)
  | cc when cc < 216  -> 6
  | cc when cc <= 222 -> 7 (* uppercase complements *)
  | cc when cc < 224  -> 8
  | cc when cc <= 246 -> 9 (* lowercase accented *)
  | cc when cc < 248  -> 10
  | cc when cc <= 254 -> 11 (* lowercase complements *)
  | _                 -> 12

let get_subrange_bounds id =
  let get_subrange_code = function
    | 0  -> (0, 64)
    | 1  -> (65, 90)
    | 2  -> (91, 96)
    | 3  -> (97, 122)
    | 4  -> (123, 191)
    | 5  -> (192, 214)
    | 6  -> (215, 215)
    | 7  -> (216, 222)
    | 8  -> (223, 223)
    | 9  -> (224, 246)
    | 10 -> (247, 247)
    | 11 -> (248, 254)
    | _  -> (255, 255)
  in let (min, max) = get_subrange_code id in (Char.chr min, Char.chr max)

let alternatives c =
  match get_subrange c with
  | 1 | 5 | 7  -> [c; Char.chr (Char.code c + 32)]
  | 3 | 9 | 11 -> [Char.chr (Char.code c - 32); c]
  | _          -> [c]

let range_alternatives start stop =
  let rec partition_range start stop =
    let sr_id = get_subrange start in
    let sr_id' = get_subrange stop in
    if sr_id = sr_id' then [(start, stop)] else
    let (_, max) = get_subrange_bounds sr_id in
    (start, max)::partition_range (Char.chr (Char.code max + 1)) stop in
  let rec get_alternatives = function
    | (min, max)::tl -> (match alternatives min, alternatives max with
                         | _::[], _::[]                     -> (min, max)::get_alternatives tl
                         | alt1::alt2::[], alt1'::alt2'::[] -> (alt1, alt1')::(alt2, alt2')::
                                                               get_alternatives tl
                         | _                                -> failwith "Malformed range")
    | []             -> [] in
  get_alternatives (partition_range start stop)

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
  | Shorthand s          -> drange_of_shorthand s
  | Single c             -> Char_range.of_list [Single c]
  | ISingle c            -> Char_range.of_list (List.map (fun c -> Single c) (alternatives c))
  | Range (start, stop)  -> Char_range.of_list [Range (start, stop)]
  | IRange (start, stop) -> Char_range.of_list (List.map (fun (min, max) -> Range (min, max))
                                                         (range_alternatives start stop))

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
  | IRegular c                -> of_main_atom level (One_of [ISingle c])
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

let find_first_matching exprs buffer =
  let rec find_first_matching_rec id = function
    | hd::tl -> let automaton = of_top_expr 1 hd in
                compose_states automaton (final_of automaton)
                               (fun (rstate, data) -> (rstate,
                                                       { data with stack = Int id::data.stack }))::
                find_first_matching_rec (id+1) tl
    | []     -> [] in
  let full_automaton = match find_first_matching_rec 0 exprs with
    | [] -> failwith "Empty RegEx list"
    | l  -> let (automaton, id) = add_state ~initial:true empty in
            let (automaton, id') = add_state automaton in
            let automaton = add_transition automaton id id' Epsilon in
            List.fold_left (fun x y -> link_ignore ~state:[id'] ~keep_final:true x y) automaton l in
  match match_first full_automaton buffer with
  | Some (_, data) -> (match List.hd data.stack with
                       | Int i -> i
                       | _ -> failwith "Corrupt stack")
  | _              -> -1
