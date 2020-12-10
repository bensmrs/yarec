open Alcotest
open Testutil
open Yarec__Util

let test_explode () =
  check (list char) "is empty" [] (explode "");
  check (list char) "explodes characters" ['f';'o';'o';'b';'a';'r'] (explode "foobar")

let test_find_index () =
  check_exception (fun () -> find_index (-1) [0;2;4;6;8;6;4;2;0]) Not_found;
  check int "finds index" 3 (find_index 6 [0;2;4;6;8;6;4;2;0])

let test_string_of_chars () =
  check string "is empty" "" (string_of_chars []);
  check string "merges characters" "foobar" (string_of_chars ['f';'o';'o';'b';'a';'r'])

let test_take () =
  check (list int) "takes everything" [0;2;4;6;8;6;4;2;0] (take [0;2;4;6;8;6;4;2;0] 10);
  check (list int) "takes what's asked" [0;2] (take [0;2;4;6;8;6;4;2;0] 2)

let test_drop () =
  check (list int) "drops everything" [] (drop [0;2;4;6;8;6;4;2;0] 10);
  check (list int) "drops what's asked" [4;6;8;6;4;2;0] (drop [0;2;4;6;8;6;4;2;0] 2)

let test_cons_to_nth () =
  check (list (list int)) "constructs the list" [[0];[2];[8;4];[6]]
                                                (cons_to_nth [[0];[2];[4];[6]] 2 8)

let test_append_to_nth () =
  check (list (list int)) "appends the item" [[0];[2];[4;8];[6]]
                                             (append_to_nth [[0];[2];[4];[6]] 2 [8])

let test_set_nth () =
  check (list int) "sets the item" [0;2;8;6] (set_nth [0;2;4;6] 2 8)

let test_repeat () =
  check (list int) "repeats nothing" [] (repeat 0 5);
  check (list int) "repeats what's asked" [1;1;1;1;1] (repeat 5 1)

let test_set_nth_safe () =
  check (list int) "sets the item" [0;2;8;6] (set_nth_safe [0;2;4;6] 2 8 (-1));
  check (list int) "fills the list and sets the item" [0;2;4;6;-1;-1;8]
                                                      (set_nth_safe [0;2;4;6] 6 8 (-1))

let test_slice () =
  check (list int) "slices the list" [4;6;8;6] (slice [0;2;4;6;8;6;4;2;0] 2 6)

let test_compose () =
  check (list int) "composes the functions" [4;6;8;10;12]
                                            (List.map ((fun x -> 2*x) @@ (fun x -> x+2))
                                                      [0;1;2;3;4])

let test_range () =
  check (list int) "constructs the range" [2;3;4;5;6] (2--6)

let tests = [
  "explode", `Quick, test_explode;
  "find_index", `Quick, test_find_index;
  "string_of_chars", `Quick, test_string_of_chars;
  "take", `Quick, test_take;
  "drop", `Quick, test_drop;
  "cons_to_nth", `Quick, test_cons_to_nth;
  "append_to_nth", `Quick, test_append_to_nth;
  "set_nth", `Quick, test_set_nth;
  "repeat", `Quick, test_repeat;
  "set_nth_safe", `Quick, test_set_nth_safe;
  "slice", `Quick, test_slice;
  "@@", `Quick, test_compose;
  "--", `Quick, test_range
]
