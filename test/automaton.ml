open Alcotest
open Option
open Testutil
open Yarec__Automaton

module Aut = Make_simple (Int_element)

let test_empty () =
  check int "has 0 states" 0 (Aut.order Aut.empty);
  check int "has 0 transitions" 0 (Aut.size Aut.empty)

let test_add_state () =
  let aut, _ = Aut.add_state Aut.empty in
  check int "has 1 state" 1 (Aut.order (aut));
  check int "has 0 transitions" 0 (Aut.size (aut));
  check int "has 0 initial states" 0 (List.length (Aut.initial aut));
  check int "has 0 final states" 0 (List.length (Aut.final aut));
  let aut, id = Aut.add_state ~initial:true Aut.empty in
  check (list int) "has 1 initial state" [id] (Aut.initial aut);
  check int "has 0 final states" 0 (List.length (Aut.final aut));
  let aut, id = Aut.add_state ~final:true Aut.empty in
  check int "has 0 initial states" 0 (List.length (Aut.initial aut));
  check (list int) "has 1 final state" [id] (Aut.final aut)

let test_single () =
  let aut = Aut.single () in
  check int "has 1 state" 1 (Aut.order aut);
  check int "has 0 transitions" 0 (Aut.size aut);
  check int "has 1 initial state" 1 (List.length (Aut.initial aut));
  check int "has 1 final state" 1 (List.length (Aut.final aut))

let test_add_transition () =
  let aut, start = Aut.add_state Aut.empty in
  let aut, stop = Aut.add_state aut in
  let aut = Aut.add_transition aut start stop Epsilon in
  check int "has 2 states" 2 (Aut.order aut);
  check int "has 1 transition" 1 (Aut.size aut);
  check int "has 0 initial states" 0 (List.length (Aut.initial aut));
  check int "has 0 final states" 0 (List.length (Aut.final aut))

let test_of_transition () =
  let aut = Aut.of_transition Epsilon in
  check int "has 2 states" 2 (Aut.order aut);
  check int "has 1 transition" 1 (Aut.size aut);
  let initial = Aut.initial aut in
  let final = Aut.final aut in
  check int "has 1 initial state" 1 (List.length initial);
  check int "has 1 final state" 1 (List.length final);
  check bool "which are different" false (initial = final)

let test_remove_final () =
  let aut = Aut.of_transition Epsilon in
  let initial = Aut.initial aut in
  let aut = Aut.remove_final aut (Aut.final aut) in
  check int "has 2 states" 2 (Aut.order (aut));
  check int "has 1 transition" 1 (Aut.size (aut));
  check (list int) "does not alter initial states" initial (Aut.initial aut);
  check int "has 0 final states" 0 (List.length (Aut.final aut))

let test_add_final () =
  let aut = Aut.of_transition Epsilon in
  let initial = Aut.initial aut in
  let final = Aut.final aut in
  let aut = Aut.remove_final aut final in
  let aut = Aut.add_final aut final in
  check int "has 2 states" 2 (Aut.order (aut));
  check int "has 1 transition" 1 (Aut.size (aut));
  check (list int) "does not alter initial states" initial (Aut.initial aut);
  check (list int) "has 1 final state" final (Aut.final aut)

let test_remove_initial () =
  let aut = Aut.of_transition Epsilon in
  let final = Aut.final aut in
  let aut = Aut.remove_initial aut (Aut.initial aut) in
  check int "has 2 states" 2 (Aut.order (aut));
  check int "has 1 transition" 1 (Aut.size (aut));
  check (list int) "does not alter final states" final (Aut.final aut);
  check int "has 0 initial states" 0 (List.length (Aut.initial aut))

let test_add_initial () =
  let aut = Aut.of_transition Epsilon in
  let initial = Aut.initial aut in
  let final = Aut.final aut in
  let aut = Aut.remove_initial aut initial in
  let aut = Aut.add_initial aut initial in
  check int "has 2 states" 2 (Aut.order (aut));
  check int "has 1 transition" 1 (Aut.size (aut));
  check (list int) "does not alter final states" final (Aut.final aut);
  check (list int) "has 1 initial state" initial (Aut.initial aut)

let diamond =
  let (aut, initial) = Aut.add_state Aut.empty in
  let (aut, level1) = Aut.add_state aut in
  let (aut, level1') = Aut.add_state aut in
  let (aut, final) = Aut.add_state aut in
  let aut = Aut.add_transition aut initial level1 (Consume 0) in
  let aut = Aut.add_transition aut initial level1' (Consume 1) in
  let aut = Aut.add_transition aut level1 final (Consume 1) in
  let aut = Aut.add_transition aut level1' final Epsilon in
  let aut = Aut.add_initial aut [initial] in
  Aut.add_final aut [final]

let single_diamond = Aut.restrict diamond

let test_restrict () =
  check int "has same order" (Aut.order diamond) (Aut.order single_diamond);
  check int "has same size" (Aut.size diamond) (Aut.size single_diamond);
  check int "has same number of initial states" (List.length (Aut.initial diamond))
                                                (List.length (Aut.initial single_diamond));
  check int "has same number of final states" (List.length (Aut.final diamond))
                                              (List.length (Aut.final single_diamond))

let test_restrict' () =
  let aut = Aut.restrict' diamond in
  check int "has order + 2" (Aut.order diamond + 2) (Aut.order aut);
  let dl_initial = List.length (Aut.initial diamond) in
  let dl_final = List.length (Aut.final diamond) in
  check int "has size + #initial + #final" (Aut.size diamond + dl_initial + dl_final)
                                           (Aut.size aut);
  check int "has same number of initial states" dl_initial (List.length (Aut.initial aut));
  check int "has same number of final states" dl_final (List.length (Aut.final aut))

let build_state buffer : Aut.runtime_state =
  ({ id = 0; buffer; start = 0; cursor = 0; steps = ([], []) }, ())

let test_check_with () =
  let chk = Aut.check_with single_diamond in
  check bool "single_diamond doesn't match []" false (is_some (chk [build_state []]));
  check bool "single_diamond doesn't match 0" false (is_some (chk [build_state [0]]));
  check bool "single_diamond matches 01" true (is_some (chk [build_state [0; 1]]));
  check bool "single_diamond doesn't match 01000" false (is_some
                                                           (chk [build_state [0; 1; 0; 0; 0]]));
  check bool "single_diamond matches 1" true (is_some (chk [build_state [1]]));
  check bool "single_diamond doesn't match 2" false (is_some (chk [build_state [2]]));
  check bool "single_diamond doesn't match 201000" false (is_some
                                                            (chk [build_state [2; 0; 1; 0; 0; 0]]));
  let chk = Aut.check_with diamond in
  check bool "diamond doesn't match []" false (is_some (chk [build_state []]));
  check bool "diamond doesn't match 0" false (is_some (chk [build_state [0]]));
  check bool "diamond matches 01" true (is_some (chk [build_state [0; 1]]));
  check bool "diamond matches 01000" true (is_some (chk [build_state [0; 1; 0; 0; 0]]));
  check bool "diamond matches 1" true (is_some (chk [build_state [1]]));
  check bool "diamond doesn't match 2" false (is_some (chk [build_state [2]]));
  check bool "diamond doesn't match 201000" false (is_some (chk [build_state [2; 0; 1; 0; 0; 0]]))

let test_check_match_first f =
  check bool "single matches []" true (f (Aut.single ()) []);
  let chk = f single_diamond in
  check bool "single_diamond doesn't match []" false (chk []);
  check bool "single_diamond doesn't match 0" false (chk [0]);
  check bool "single_diamond matches 01" true (chk [0; 1]);
  check bool "single_diamond doesn't match 01000" false (chk [0; 1; 0; 0; 0]);
  check bool "single_diamond matches 1" true (chk [1]);
  check bool "single_diamond doesn't match 2" false (chk [2]);
  check bool "single_diamond doesn't match 201000" false (chk [2; 0; 1; 0; 0; 0]);
  let chk = f diamond in
  check bool "diamond doesn't match []" false (chk []);
  check bool "diamond doesn't match 0" false (chk [0]);
  check bool "diamond matches 01" true (chk [0; 1]);
  check bool "diamond matches 01000" true (chk [0; 1; 0; 0; 0]);
  check bool "diamond matches 1" true (chk [1]);
  check bool "diamond doesn't match 2" false (chk [2]);
  check bool "diamond matches 201000" true (chk [2; 0; 1; 0; 0; 0])

let test_match_first () =
  test_check_match_first (fun aut buf -> match Aut.match_first aut buf with
                                         | Some _ -> true
                                         | None   -> false)

let test_check () =
  test_check_match_first Aut.check

let test_link () =
  let (aut, initial) = Aut.add_state Aut.empty in
  let (aut, level1) = Aut.add_state aut in
  let (aut, level1') = Aut.add_state aut in
  let (aut, final) = Aut.add_state aut in
  let aut = Aut.add_transition aut initial level1 (Consume 0) in
  let aut = Aut.add_transition aut initial level1' (Consume 1) in
  let aut = Aut.add_transition aut level1 final (Consume 1) in
  let aut = Aut.add_transition aut level1' final Epsilon in
  let aut = Aut.add_initial aut [initial] in
  let aut = Aut.add_final aut [final] in
  let (aut', trans) = Aut.link aut aut in
  let aut' = Aut.restrict aut' in
  check int "has 7 states" 7 (Aut.order (aut'));
  check int "has 8 transitions" 8 (Aut.size (aut'));
  check (list int) "has 1 initial state" [initial] (Aut.initial aut');
  check (list int) "has 1 final state" [Hashtbl.find trans final] (Aut.final aut');
  check bool "doesn't match 01" false (Aut.check aut' [0; 1]);
  check bool "matches 0101" true (Aut.check aut' [0; 1; 0; 1]);
  check bool "matches 011" true (Aut.check aut' [0; 1; 1]);
  check bool "doesn't match 1" false (Aut.check aut' [1]);
  check bool "doesn't match 10" false (Aut.check aut' [1; 0]);
  check bool "matches 11" true (Aut.check aut' [1; 1]);
  let (aut', trans) = Aut.link ~keep_final:true aut aut in
  let aut' = Aut.restrict aut' in
  check int "has 7 states" 7 (Aut.order (aut'));
  check int "has 8 transitions" 8 (Aut.size (aut'));
  check (list int) "has 1 initial state" [initial] (Aut.initial aut');
  check (list int) "has 2 final states" [final; Hashtbl.find trans final] (Aut.final aut');
  check bool "matches 01" true (Aut.check aut' [0; 1]);
  check bool "matches 0101" true (Aut.check aut' [0; 1; 0; 1]);
  check bool "matches 011" true (Aut.check aut' [0; 1; 1]);
  check bool "matches 1" true (Aut.check aut' [1]);
  check bool "matches 10" true (Aut.check aut' [1; 0]);
  check bool "matches 11" true (Aut.check aut' [1; 1]);
  let (aut', trans) = Aut.link ~state:[level1; level1'] aut aut in
  let aut' = Aut.restrict aut' in
  check int "has 7 states" 7 (Aut.order (aut'));
  check int "has 10 transitions" 10 (Aut.size (aut'));
  check (list int) "has 1 initial state" [initial] (Aut.initial aut');
  check (list int) "has 1 final state" [Hashtbl.find trans final] (Aut.final aut');
  check bool "matches 01" true (Aut.check aut' [0; 1]);
  check bool "doesn't match 0101" false (Aut.check aut' [0; 1; 0; 1]);
  check bool "doesn't match 011" false (Aut.check aut' [0; 1; 1]);
  check bool "doesn't match 1" false (Aut.check aut' [1]);
  check bool "doesn't match 10" false (Aut.check aut' [1; 0]);
  check bool "matches 101" true (Aut.check aut' [1; 0; 1]);
  check bool "matches 11" true (Aut.check aut' [1; 1]);
  check_exception (fun () -> Aut.link ~state':[level1; level1'] aut aut) Illegal_link

let test_link_ignore () =
  let aut = Aut.restrict (Aut.link_ignore diamond diamond) in
  check int "has 7 states" 7 (Aut.order (aut));
  check int "has 8 transitions" 8 (Aut.size (aut));
  check int "has 1 initial state" 1 (List.length (Aut.initial aut));
  check int "has 1 final state" 1 (List.length (Aut.final aut));
  check bool "doesn't match 01" false (Aut.check aut [0; 1]);
  check bool "matches 0101" true (Aut.check aut [0; 1; 0; 1]);
  check bool "matches 011" true (Aut.check aut [0; 1; 1]);
  check bool "doesn't match 1" false (Aut.check aut [1]);
  check bool "doesn't match 10" false (Aut.check aut [1; 0]);
  check bool "matches 11" true (Aut.check aut [1; 1])

let check_diamond_repeated_thrice aut =
  check bool "doesn't match []" false (Aut.check aut []);
  check bool "doesn't match 0101" false (Aut.check aut [0; 1; 0; 1]);
  check bool "matches 010101" true (Aut.check aut [0; 1; 0; 1; 0; 1]);
  check bool "doesn't match 01010101" false (Aut.check aut [0; 1; 0; 1; 0; 1; 0; 1]);
  check bool "matches 01101" true (Aut.check aut [0; 1; 1; 0; 1]);
  check bool "doesn't match 11" false (Aut.check aut [1; 1]);
  check bool "matches 111" true (Aut.check aut [1; 1; 1])

let test_repeat () =
  check_exception' (fun () -> Aut.repeat diamond (-1)) "Invalid_argument";
  let zero = Aut.repeat diamond 0 in
  check bool "returns a single-state automaton" true (Aut.order zero = 1 && Aut.size zero = 0);
  check bool "returns the argument unchanged" true (diamond == Aut.repeat diamond 1);
  let aut = Aut.restrict (Aut.repeat diamond 3) in
  check_diamond_repeated_thrice aut

let test_chain () =
  let aut = Aut.restrict' (Aut.chain diamond) in
  check bool "doesn't match []" false (Aut.check aut []);
  check bool "matches 0101" true (Aut.check aut [0; 1; 0; 1]);
  check bool "matches 010101" true (Aut.check aut [0; 1; 0; 1; 0; 1]);
  check bool "matches 01010101" true (Aut.check aut [0; 1; 0; 1; 0; 1; 0; 1]);
  check bool "matches 01101" true (Aut.check aut [0; 1; 1; 0; 1]);
  check bool "matches 11" true (Aut.check aut [1; 1]);
  check bool "matches 111" true (Aut.check aut [1; 1; 1])

let test_from_to () =
  check_exception' (fun () -> Aut.from_to diamond 3 2) "Invalid_argument";
  let aut = Aut.restrict (Aut.from_to diamond 3 3) in
  check_diamond_repeated_thrice aut;
  let aut = Aut.restrict (Aut.from_to diamond 2 3) in
  check bool "doesn't match []" false (Aut.check aut []);
  check bool "doesn't match 01" false (Aut.check aut [0; 1]);
  check bool "matches 0101" true (Aut.check aut [0; 1; 0; 1]);
  check bool "matches 010101" true (Aut.check aut [0; 1; 0; 1; 0; 1]);
  check bool "matches 01010101" true (Aut.check aut [0; 1; 0; 1; 0; 1; 0; 1]);
  check bool "matches 01101" true (Aut.check aut [0; 1; 1; 0; 1]);
  check bool "doesn't match 1" false (Aut.check aut [1]);
  check bool "matches 11" true (Aut.check aut [1; 1]);
  check bool "matches 111" true (Aut.check aut [1; 1; 1])

let test_loop () =
  let aut = Aut.restrict' (Aut.loop diamond) in
  check bool "matches []" true (Aut.check aut []);
  check bool "doesn't match 0" false (Aut.check aut [0]);
  check bool "matches 01" true (Aut.check aut [0; 1]);
  check bool "matches 0101" true (Aut.check aut [0; 1; 0; 1]);
  check bool "match 01010101" true (Aut.check aut [0; 1; 0; 1; 0; 1; 0; 1]);
  check bool "matches 01101" true (Aut.check aut [0; 1; 1; 0; 1]);
  check bool "matches 1" true (Aut.check aut [1]);
  check bool "matches 11" true (Aut.check aut [1; 1])

let test_reverse () =
  (* This very beta function is left unchecked *) ()

let test_match_one () =
  (* This function is left unchecked because in such a simple automaton kind, it is useless *) ()

let tests = [
  "empty", `Quick, test_empty;
  "add_state", `Quick, test_add_state;
  "single", `Quick, test_single;
  "remove_final", `Quick, test_remove_final;
  "add_final", `Quick, test_add_final;
  "remove_initial", `Quick, test_remove_initial;
  "add_initial", `Quick, test_add_initial;
  "restrict", `Quick, test_restrict;
  "restrict'", `Quick, test_restrict';
  "check_with", `Quick, test_check_with;
  "match_first", `Quick, test_match_first;
  "check", `Quick, test_check;
  "link", `Quick, test_link;
  "link_ignore", `Quick, test_link_ignore;
  "repeat", `Quick, test_repeat;
  "chain", `Quick, test_chain;
  "from_to", `Quick, test_from_to;
  "loop", `Quick, test_loop;
  "reverse", `Quick, test_reverse;
  "match_one", `Quick, test_match_one
]
