exception Not_a_range
type 'a item = Single of 'a | Range of 'a * 'a

module Make (E : sig
                   type t
                   val compare : t -> t -> int
                   val consecutive : t -> t -> bool
                 end) = struct

  type elt = E.t
  type t = elt item list * elt item list

  let item_max e e' = match E.compare e e' with
    | i when i >= 0 -> e
    | _             -> e'

  let sort e e' = match e, e' with
    | Range (x, y), Range (x', y') when E.compare x x' = 0 -> E.compare y' y
    | Range (x, _), Range (x', _) | Single x, Single x'    -> E.compare x x'
    | Range (_, _), Single _                               -> -1
    | Single _, Range (_, _)                               -> 1

  let simplify (ranges, singles) =
    let rec simplify_ranges = function
      | [] | _::[] as l -> l
      | Range (start, stop)::Range (start', stop')::tl when E.compare stop start' >= 0
                        -> simplify_ranges (Range (start, item_max stop stop')::tl)
      | Range (start, stop)::Range (start', stop')::tl when E.consecutive stop start'
                        -> simplify_ranges (Range (start, stop')::tl)
      | hd::tl          -> hd::simplify_ranges tl in
    let rec simplify_both ranges singles acc_r acc_s = match ranges, singles with
      | _, Single e::Single e'::tl when E.compare e e' = 0
                  -> simplify_both ranges (Single e::tl) acc_r acc_s
      | Range (start, stop)::_, Single e::tl when E.compare e start >= 0 && E.compare stop e >= 0
                  -> simplify_both ranges tl acc_r acc_s
      | Range (start, stop)::tl, Single e::tl' when E.consecutive stop e
                  -> simplify_both (Range (start, e)::tl) tl' acc_r acc_s
      | Range (_, stop) as hd::tl, Single e::_ when E.compare e stop > 0
                  -> simplify_both tl singles (hd::acc_r) acc_s
      | _, Single e::Single e'::tl when E.consecutive e e'
                  -> simplify_both (Range (e, e')::ranges) tl acc_r acc_s
      | _, hd::tl -> simplify_both ranges tl acc_r (hd::acc_s)
      | _, []     -> (List.rev_append acc_r ranges, List.rev_append acc_s singles) in
    let (r, s) = simplify_both (simplify_ranges ranges) singles [] [] in
    (simplify_ranges r, s)

  let of_list l =
    simplify (List.partition (function Single _ -> false | _ -> true) (List.sort sort l))

  let add_list (r, s) l = of_list [r; s; l]

  let add (r, s) (r', s') = of_list [r; r'; s; s']

  let empty = of_list []

  let rec mem e (ranges, singles) =
    let rec singles_have e = function
      | Single x::tl when E.compare e x > 0 -> singles_have e tl
      | Single x::tl when E.compare x e > 0 -> false
      | []                                  -> false
      | _                                   -> true in
    let rec ranges_have e = function
      | Range (_, x)::tl when E.compare e x > 0 -> ranges_have e tl
      | Range (x, _)::tl when E.compare x e > 0 -> singles_have e singles
      | []                                      -> singles_have e singles
      | _                                       -> true in
    ranges_have e ranges
end

module Int_range = Make (struct
                           type t = int
                           let compare = compare
                           let consecutive x y = y - x = 1
                         end)


module Char_range = Make (struct
                            type t = char
                            let compare = compare
                            let consecutive x y = Char.code y - Char.code x = 1
                          end)
