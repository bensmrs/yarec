type quantifier_t = From_to of int * int
                  | From of int
                  | Exactly of int

type qualifier_t = Greedy | Lazy | Possessive

type one_atom_t = Single of char
                | Range of char * char

type kind = Positive | Negative

type main_atom_t = Special of string
                 | Regular of char
                 | One_of of one_atom_t list
                 | None_of of one_atom_t list
                 | Look_ahead of kind * t
                 | Look_behind of kind * t
                 | No_capture of t
                 | Capture of int * t
                 | Back_ref of int

and quantified_t = Quantified of main_atom_t * qualifier_t * quantifier_t
                 | Start_of_line
                 | End_of_line

and t = quantified_t list

open Printf

let string_of_quantifier_t = function
  | From_to(min,max) -> sprintf "%i,%i" min max
  | From min -> sprintf ">%i" min
  | Exactly nb -> sprintf "=%i" nb

let string_of_qualifier_t = function
  | Greedy -> "G"
  | Lazy -> "L"
  | Possessive -> "P"

let string_of_one_atom_t = function
  | Single c -> sprintf "'%c'" c
  | Range(c1,c2) -> sprintf "'%c'-'%c'" c1 c2

let string_of_kind = function
  | Positive -> "+"
  | Negative -> "-"

let rec string_of_main_atom_t = function
  | Special s -> sprintf "S%s" s
  | Regular c -> sprintf "'%C'" c
  | One_of oal -> sprintf "[%s]" (String.concat " " (List.map string_of_one_atom_t oal))
  | None_of oal -> sprintf "[^%s]" (String.concat " " (List.map string_of_one_atom_t oal))
  | Look_ahead(k,t) -> sprintf "(%s)_{>%s}" (string_of t) (string_of_kind k)
  | Look_behind(k,t) -> sprintf "(%s)_{<%s}" (string_of t) (string_of_kind k)
  | No_capture t -> sprintf "(%s)_{}" (string_of t)
  | Capture(num,t) -> sprintf "(%s)_{%i}" (string_of t) num
  | Back_ref num -> sprintf "<%i>" num

and string_of_quantified_t = function
  | Quantified(main,Greedy,Exactly 1) -> string_of_main_atom_t main
  | Quantified(main,Greedy,quant) -> sprintf "%s_{%s}" (string_of_main_atom_t main) (string_of_quantifier_t quant)
  | Quantified(main,qualif,Exactly 1) -> sprintf "%s_{%s}" (string_of_main_atom_t main) (string_of_qualifier_t qualif)
  | Quantified(main,qualif,quant) -> sprintf "%s_{%s}_{%s}" (string_of_main_atom_t main) (string_of_qualifier_t qualif) (string_of_quantifier_t quant)
  | Start_of_line -> "'^'"
  | End_of_line -> "'$'"

and string_of l = String.concat ";" (List.map string_of_quantified_t l)
