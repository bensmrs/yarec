let string_of_exception exc = List.hd (String.split_on_char '(' (Printexc.to_string_default exc))

let check_exception expr exc =
  Alcotest.check Alcotest.bool "raises exception" true
    (try ignore (expr ()); false with
     | e when e == exc -> true
     | _              -> false)

let check_exception' expr str =
  Alcotest.check Alcotest.bool "raises exception" true
    (try ignore (expr ()); false with
     | e when (string_of_exception e) = str -> true
     | _              -> false)
