module A = Alcotest
open A
open Yarec__Regex

let test_check () =
  A.check bool "terminates" true (check (compile "(([^bc]*))*c") "aaaaaaaaaaaaaaaaaaaaaaaaaaaaabc");
  A.check bool "terminates" true (check (compile "(?:a?){20}a{20}") "aaaaaaaaaaaaaaaaaaaa");
  A.check bool "checks foobar" true (check (compile "foo(?:bar|baz)") "foobar");
  A.check bool "checks foobar" true (check (compile "foo(?:baz|bar)") "foobar");
  A.check bool "checks foobarbaz" true (check (compile "^foobar") "foobarbaz");
  A.check bool "doesn't check bazfoobar" false (check (compile "^foobar") "bazfoobar");
  A.check bool "checks bazfoobar" true (check (compile "foobar$") "bazfoobar");
  A.check bool "doesn't check foobarbaz" false (check (compile "foobar$") "foobarbaz");
  A.check bool "checks aabbb" true (check (compile "a{2}b{3}") "aabbb");
  A.check bool "checks foobarfoo" true (check (compile "^(.*)bar\\1") "foobarfoo");
  A.check bool "doesn't check foobarbaz" false (check (compile "^(.*)bar\\1") "foobarbaz");
  A.check bool "checks foobar" true (check (compile "(?<=foo)bar$") "foobar");
  A.check bool "doesn't check foobar" false (check (compile "(?<!foo)bar$") "foobar");
  A.check bool "checks foobar" true (check (compile "^foo(?=bar)") "foobar");
  A.check bool "doesn't check foobar" false (check (compile "^foo(?!bar)") "foobar")

let test_find_first_matching () =
  let regexes = List.map compile ["^foo(bar)"; "^ding[do]ng?$"; "testtest"] in
  Alcotest.check int "finds the right index" 1 (find_first_matching regexes "dingon")

let test_match_one () =
  A.check (option (list string)) "matches caseless greedily" (Some ["Ababababab"; "Abababab"; "ab"])
          (match_one (compile ~flags:[`CASELESS] "((ab)*)ab") "Ababababab");
  A.check (option (list string)) "matches caseless lazily" (Some ["Abab"; "Ab"; "Ab"])
          (match_one (compile ~flags:[`CASELESS] "((ab)?)ab") "Ababababab");
  A.check (option (list string)) "matches caseless strange range" (Some ["A"])
          (match_one (compile ~flags:[`CASELESS] "[Z-b]") "A");
  A.check (option (list string)) "matches only foo" (Some ["foo"])
          (match_one (compile "^foo(?=bar)") "foobar");
  A.check (option (list string)) "matches strange backref" (Some ["foobarfoo"; "foo"; "foo"])
          (match_one (compile "^(.*)bar(\\1)") "foobarfoo");
  A.check (option (list string)) "doesn't match newline" (Some ["foo"])
          (match_one (compile ".*") "foo\nbar");
  A.check (option (list string)) "matches newline" (Some ["foo\nbar"])
          (match_one (compile ~flags:[`DOTALL] ".*") "foo\nbar");
  A.check (option (list string)) "doesn't match after newline" None
          (match_one (compile "^bar") "foo\nbar");
  A.check (option (list string)) "matches after newline" (Some ["bar"])
          (match_one (compile ~flags:[`MULTILINE] "^bar") "foo\nbar");
  A.check (option (list string)) "doesn't match before newline" None
          (match_one (compile "foo$") "foo\nbar");
  A.check (option (list string)) "matches before newline" (Some ["foo"])
          (match_one (compile ~flags:[`MULTILINE] "foo$") "foo\nbar")

let tests = [
  "check", `Quick, test_check;
  "find_first_matching", `Quick, test_find_first_matching;
  "match_one", `Quick, test_match_one
]
