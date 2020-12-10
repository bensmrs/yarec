open Alcotest
open Yarec__Drange
open Yarec__Util

let test_mem () =
  let range = Int_range.of_list [Single 1; Range (3,10); Single 5; Single 0] in
  check (list int) "accepts the right elements" (0::1::3--10)
                                                (List.filter (fun i -> Int_range.mem i range)
                                                             (-1--11))

let test_add_list () =
  let range = Int_range.add_list (Int_range.of_list [Single 1; Range (3,10)])
                                 [Single 5; Single 0] in
  check (list int) "accepts the right elements" (0::1::3--10)
                                                (List.filter (fun i -> Int_range.mem i range)
                                                             (-1--11))

let test_add () =
  let range = Int_range.add (Int_range.of_list [Single 1; Range (3,10)])
                            (Int_range.of_list [Single 5; Single 0]) in
  check (list int) "accepts the right elements" (0::1::3--10)
                                                (List.filter (fun i -> Int_range.mem i range)
                                                             (-1--11))

let test_substract () =
  let range = Int_range.substract (Int_range.of_list [Range (0,10); Single 15; Range (20,25)])
                                  (Int_range.of_list [Range (5,15); Single 22]) in
  check (list int) "accepts the right elements" (0--4@20--21@23--25)
                                                (List.filter (fun i -> Int_range.mem i range)
                                                             (-1--26))

let tests = [
  "mem", `Quick, test_mem;
  "add_list", `Quick, test_add_list;
  "add", `Quick, test_add;
  "substract", `Quick, test_substract
]
