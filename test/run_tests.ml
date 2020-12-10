let test_suites: unit Alcotest.test list = [
  "Automaton", Automaton.tests;
  "Drange", Int_range.tests;
  "Regex", Regex.tests;
  "Util", Util.tests
]

let () = Alcotest.run "proj" test_suites
