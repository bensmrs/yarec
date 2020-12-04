type shorthand = H_space | Non_h_space
               | V_space | Non_v_space
               | White_space | Non_white_space
               | Digit | Non_digit
               | Word_char | Non_word_char
               | New_line | Non_new_line
               | Any_char

type quantifier_t = From_to of int * int
                  | From of int
                  | Exactly of int

type qualifier_t = Greedy | Lazy | Possessive

type one_atom_t = Shorthand of shorthand
                | Single of char
                | Range of char * char

type kind = Positive | Negative

type main_atom_t = Special of shorthand
                 | Regular of char
                 | One_of of one_atom_t list
                 | None_of of one_atom_t list
                 | Look_ahead of kind * t
                 | Look_behind of kind * t
                 | No_capture of t
                 | Capture of t
                 | Back_ref of int

and quantified_t = Quantified of main_atom_t * qualifier_t * quantifier_t
                 | Start_of_line
                 | End_of_line
                 | Start_of_input
                 | End_of_input

and main_expr_t = quantified_t list

and t = Expr of main_expr_t
      | Either of main_expr_t * t

open Printf

let string_of_shorthand = function
  | H_space             -> "hspace"
  | Non_h_space         -> "nonhspace"
  | V_space             -> "vspace"
  | Non_v_space         -> "nonvspace"
  | White_space         -> "whitespace"
  | Non_white_space     -> "nonwhitespace"
  | Digit               -> "digit"
  | Non_digit           -> "nondigit"
  | Word_char           -> "wordchar"
  | Non_word_char       -> "nonwordchar"
  | New_line            -> "newline"
  | Non_new_line        -> "nonnewline"
  | Any_char            -> "_"

let string_of_quantifier_t = function
  | From_to(min,max) -> sprintf "%i,%i" min max
  | From 0 -> "*"
  | From 1 -> "+"
  | From min -> sprintf ">=%i" min
  | Exactly nb -> sprintf "=%i" nb

let string_of_qualifier_t = function
  | Greedy -> "G"
  | Lazy -> "L"
  | Possessive -> "P"

let string_of_one_atom_t = function
  | Single c -> sprintf "'%c'" c
  | Shorthand s -> string_of_shorthand s
  | Range(c1,c2) -> sprintf "'%c'-'%c'" c1 c2

let string_of_kind = function
  | Positive -> "+"
  | Negative -> "-"

let rec string_of_main_atom_t = function
  | Special s -> string_of_shorthand s
  | Regular c -> sprintf "%C" c
  | One_of oal -> sprintf "[%s]" (String.concat " " (List.map string_of_one_atom_t oal))
  | None_of oal -> sprintf "[^%s]" (String.concat " " (List.map string_of_one_atom_t oal))
  | Look_ahead(k,t) -> sprintf "(%s)_{->%s}" (string_of t) (string_of_kind k)
  | Look_behind(k,t) -> sprintf "(%s)_{<-%s}" (string_of t) (string_of_kind k)
  | No_capture t -> sprintf "(%s)_{}" (string_of t)
  | Capture t -> sprintf "(%s)" (string_of t)
  | Back_ref num -> sprintf "<%i>" num

and string_of_quantified_t = function
  | Quantified(main,Greedy,Exactly 1) -> string_of_main_atom_t main
  | Quantified(main,Greedy,quant) -> sprintf "%s_{%s}" (string_of_main_atom_t main) (string_of_quantifier_t quant)
  | Quantified(main,qualif,Exactly 1) -> sprintf "%s_{%s}" (string_of_main_atom_t main) (string_of_qualifier_t qualif)
  | Quantified(main,qualif,quant) -> sprintf "%s_{%s}_{%s}" (string_of_main_atom_t main) (string_of_qualifier_t qualif) (string_of_quantifier_t quant)
  | Start_of_line -> "'^'"
  | End_of_line -> "'$'"
  | Start_of_input -> "'^'"
  | End_of_input -> "'$'"

and string_of_main_expr_t l = String.concat ";" (List.map string_of_quantified_t l)

and string_of = function
  | Expr e -> string_of_main_expr_t e
  | Either(e,m) -> sprintf "%s | %s" (string_of_main_expr_t e) (string_of m)
