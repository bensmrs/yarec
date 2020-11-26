exception Not_a_range
type 'a item = Single of 'a | Range of 'a * 'a

module Make (E : sig
                   type t
                   val compare : t -> t -> int
                   val succ : t -> t
                   val prev : t -> t
                 end) = struct

  type elt = E.t
  type t = elt item list * elt item list

  let item_max e e' = match E.compare e e' with
    | i when i >= 0 -> e
    | _             -> e'

  let item_min e e' = match E.compare e e' with
    | i when i >= 0 -> e'
    | _             -> e

  let sort e e' = match e, e' with
    | Range (x, y), Range (x', y') when E.compare x x' = 0 -> E.compare y' y
    | Range (x, _), Range (x', _) | Single x, Single x'    -> E.compare x x'
    | Range (_, _), Single _                               -> -1
    | Single _, Range (_, _)                               -> 1

  let consecutive e e' = E.compare (E.succ e) e' = 0

  let simplify (ranges, singles) =
    let rec simplify_ranges = function
      | [] | _::[] as l -> l
      | Range (start, stop)::Range (start', stop')::tl when E.compare stop start' >= 0
                        -> simplify_ranges (Range (start, item_max stop stop')::tl)
      | Range (start, stop)::Range (start', stop')::tl when consecutive stop start'
                        -> simplify_ranges (Range (start, stop')::tl)
      | hd::tl          -> hd::simplify_ranges tl in
    let rec simplify_both ranges singles acc_r acc_s = match ranges, singles with
      | _, Single e::Single e'::tl when E.compare e e' = 0
                  -> simplify_both ranges (Single e::tl) acc_r acc_s
      | Range (start, stop)::_, Single e::tl when E.compare e start >= 0 && E.compare stop e >= 0
                  -> simplify_both ranges tl acc_r acc_s
      | Range (start, stop)::tl, Single e::tl' when consecutive stop e
                  -> simplify_both (Range (start, e)::tl) tl' acc_r acc_s
      | Range (_, stop) as hd::tl, Single e::_ when E.compare e stop > 0
                  -> simplify_both tl singles (hd::acc_r) acc_s
      | _, Single e::Single e'::tl when consecutive e e'
                  -> simplify_both (Range (e, e')::ranges) tl acc_r acc_s
      | _, hd::tl -> simplify_both ranges tl acc_r (hd::acc_s)
      | _, []     -> (List.rev_append acc_r ranges, List.rev_append acc_s singles) in
    let rec simplify_ranges' r s = match r with
      | []     -> ([], List.sort sort s)
      | Range (start, stop)::Range (start', stop')::tl when E.compare stop start' >= 0
               -> simplify_ranges' (Range (start, item_max stop stop')::tl) s
      | Range (start, stop)::Range (start', stop')::tl when consecutive stop start'
               -> simplify_ranges' (Range (start, stop')::tl) s
      | Range (start, stop)::tl when E.compare start stop = 0
               -> simplify_ranges' tl (Single start::s)
      | hd::tl -> let (r, s) = simplify_ranges' tl s in (hd::r, s) in
    let (r, s) = simplify_both (simplify_ranges ranges) singles [] [] in
    simplify_ranges' r s

  let of_list l =
    simplify (List.partition (function Single _ -> false | _ -> true) (List.sort sort l))

  let add_list (r, s) l = of_list [r; s; l]

  let add (r, s) (r', s') = of_list [r; r'; s; s']

  let substract (r, s) (r', s') =
    let rec minus_ranges r r' acc = match r, r' with
      | Range (start, stop)::tl, Range (start', stop')::_
          when E.compare start start' >= 0 && E.compare stop' stop >= 0
              -> minus_ranges tl r' acc
      | Range (start, stop)::tl, Range (start', stop')::tl'
          when E.compare start start' >= 0 && E.compare stop stop' > 0
              -> minus_ranges (Range (item_max start (E.succ stop'), stop)::tl) tl' acc
      | Range (start, stop)::tl, Range (start', stop')::tl'
          when E.compare start' start > 0 && E.compare stop' stop >= 0
              -> minus_ranges (Range (start, item_min stop (E.prev start'))::tl) tl' acc
      | Range (start, stop)::tl, Range (start', stop')::tl'
          when E.compare start' start > 0 && E.compare stop stop' > 0
              -> minus_ranges (Range (start, start')::Range (stop', stop)::tl) tl' acc
      | Range (_, stop) as hd::tl, Range (start, _)::_ when E.compare start stop > 0
              -> minus_ranges tl r' (hd::acc)
      | Range (start, _)::_, Range (_, stop)::tl when E.compare start stop > 0
              -> minus_ranges r tl acc
      | [], _ -> List.rev acc
      | _, [] -> List.rev_append acc r
      | _, _  -> failwith "0Unreachable branch" in
    let rec minus_cross s r acc = match s, r with
      | Single e as hd::tl, Range (start, _)::_ when E.compare start e > 0
              -> minus_cross tl r (hd::acc)
      | Single e::tl, Range (start, stop)::_ when E.compare e start >= 0 && E.compare stop e >= 0
              -> minus_cross tl r acc
      | Single e::_, Range (_, stop)::tl when E.compare e stop > 0
              -> minus_cross s tl acc
      | [], _ -> List.rev acc
      | _, [] -> List.rev_append acc s
      | _, _  -> failwith "1Unreachable branch" in
    let rec minus_cross' r s acc = match r, s with
      | Range (start, stop)::_, Single e::tl when E.compare start e > 0
              -> minus_cross' r tl acc
      | Range (start, stop)::tl, Single e::tl' when E.compare start e = 0
              -> minus_cross' (Range (E.succ e, stop)::tl) tl' acc
      | Range (start, stop)::tl, Single e::tl' when E.compare e start > 0 && E.compare stop e > 0
              -> minus_cross' (Range (start, E.prev e)::Range (E.succ e, stop)::tl) tl' acc
      | Range (start, stop)::tl, Single e::tl' when E.compare stop e = 0
              -> minus_cross' (Range (start, E.prev e)::tl) tl' acc
      | Range (_, stop) as hd::tl, Single e::_ when E.compare e stop > 0
              -> minus_cross' tl s (hd::acc)
      | [], _ -> List.rev acc
      | _, [] -> List.rev_append acc r
      | _, _  -> failwith "2Unreachable branch" in
    let rec minus_singles s s' acc = match s, s' with
      | Single e::_, Single e'::tl when E.compare e e' > 0       -> minus_singles s tl acc
      | Single e::tl, Single e'::tl' when E.compare e e' = 0     -> minus_singles tl tl' acc
      | Single e as hd::tl, Single e'::_ when E.compare e' e > 0 -> minus_singles tl s' (hd::acc)
      | [], _                                                    -> List.rev acc
      | _, []                                                    -> List.rev_append acc s
      | _, _  -> failwith "3Unreachable branch" in
    let (r, s) = (minus_ranges r r' [], minus_cross s r' []) in
    let (r, s) = (minus_cross' r s' [], minus_singles s s' []) in
    simplify (r, s)

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
                           let succ e = e + 1
                           let prev e = e - 1
                         end)

module Char_range = Make (struct
                            type t = char
                            let compare = compare
                            let consecutive x y = Char.code y - Char.code x = 1
                            let succ e = Char.chr (Char.code e + 1)
                            let prev e = Char.chr (Char.code e - 1)
                          end)
