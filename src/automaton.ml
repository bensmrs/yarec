open Util

type greed = Greedy | Lazy
type step = { greed: greed;
              start: int;
              stop: int option;
              initial_state: int;
              start_state: int option;
              depth: int }
type 'a transition = Consume of 'a | Epsilon | Eta

exception Illegal_link

module Nothing = struct
  type t = unit
  let empty = ()
end

module Int_element = struct
  type t = int
  type elt = t
  let mem = (=)
end

module Make (E : sig
                   type elt
                   type t
                   val mem : elt -> t -> bool
                 end)
            (Data : sig
                      type t
                      val empty : t
                    end) = struct

  type data = Data.t
  type buffer = E.elt list
  type generic_state = { id: int;
                         buffer: buffer;
                         start: int;
                         cursor: int;
                         steps: step list * step list }
  type runtime_state = generic_state * data
  type automaton_state = (runtime_state -> runtime_state)
  type transition_item = E.t transition
  type t = { initial: int list;
             final: int list;
             states: automaton_state list;
             transitions: (transition_item * int) list list }
  module Int_set = Set.Make (Int)

  let empty = { initial = [];
                final = [];
                states = [];
                transitions = [] }

  let next_available_state automaton = List.length automaton.states

  let add_state ?(f=fun x->x) ?(initial=false) ?(final=false) automaton =
    let id = next_available_state automaton in
    ({ initial = (if initial then id::automaton.initial else automaton.initial);
       final = (if final then id::automaton.final else automaton.final);
       states = automaton.states @ [f];
       transitions = automaton.transitions @ [[]] }, id)

  let single ?(f=fun x->x) () =
    let (a, id) = add_state ~f empty in { a with initial = [id]; final = [id] }

  let add_transition automaton start stop item =
    { automaton with transitions = cons_to_nth automaton.transitions start (item, stop) }

  let of_transition ?(f=fun x -> x) item =
    let (automaton, id) = add_state empty in
    let (automaton, id') = add_state ~f automaton in
    let automaton = add_transition automaton id id' item in
    { automaton with initial = [id]; final = [id'] }

  let remove_final automaton final =
    { automaton with final = Int_set.elements (Int_set.diff (Int_set.of_list automaton.final)
                                                            (Int_set.of_list final)) }

  let add_final automaton final =
    { automaton with final = Int_set.elements (Int_set.union (Int_set.of_list automaton.final)
                                                             (Int_set.of_list final)) }

  let remove_initial automaton initial =
    { automaton with initial = Int_set.elements (Int_set.diff (Int_set.of_list automaton.initial)
                                                              (Int_set.of_list initial)) }

  let add_initial automaton initial =
    { automaton with initial = Int_set.elements (Int_set.union (Int_set.of_list automaton.initial)
                                                               (Int_set.of_list initial)) }

  let compose_states automaton states f =
    { automaton with states = List.fold_left (fun states id ->
                                                set_nth states id (f @@ (List.nth states id)))
                                             automaton.states states }

  let link ?(state=[]) ?(state'=[]) ?(keep_final=false) automaton automaton' =
    let state = List.sort compare (if state = [] then automaton.final else state) in
    let state' = List.sort compare (if state' = [] then automaton'.initial else state') in
    if List.length state' > 1 then raise Illegal_link else
    let trans = Hashtbl.create (next_available_state automaton') in
    ignore (List.map (fun s -> List.map (fun s' -> Hashtbl.add trans s' s) state') state);
    let rec merge_states automaton ?(n=0) = function
      | f::tl when List.mem n state'
          -> let id = Hashtbl.find trans n in
             merge_states (compose_states automaton [id] f) ~n:(n+1) tl
      | f::tl
          -> let id = next_available_state automaton in
                      (Hashtbl.add trans n id;
                       merge_states { automaton with states = automaton.states @ [f];
                                                     transitions = automaton.transitions @ [[]] }
                                    ~n:(n+1) tl)
      | [] -> automaton in
    let rec merge_transitions automaton ?(n=0) transitions =
      let rec update_transition = function
        | (item, id)::tl -> (List.map (fun id' -> (item, id')) (Hashtbl.find_all trans id)) @
                            (update_transition tl)
        | []             -> [] in
      match transitions with
        | hd::tl -> let transition = update_transition hd in
                    let transitions' = List.fold_left (fun t n' -> append_to_nth t n' transition)
                                                      automaton.transitions
                                                      (Hashtbl.find_all trans n) in
                    merge_transitions { automaton with transitions = transitions' } ~n:(n+1) tl
        | []     -> automaton in
    let automaton'' = merge_transitions (merge_states automaton automaton'.states)
                                        automaton'.transitions in
    let automaton'' = if keep_final then automaton''
                                    else remove_final automaton'' automaton.final in
    (add_final automaton'' (List.fold_left (fun f n -> f @ (Hashtbl.find_all trans n))
                                            [] automaton'.final), trans)

  let link_ignore ?(state=[]) ?(state'=[]) ?(keep_final=false) automaton automaton' =
    let (a, _) = link ~state ~state' ~keep_final automaton automaton' in a

  let repeat automaton ?(initial=[]) ?(final=[]) n =
    let initial = List.sort compare (if initial = [] then automaton.initial else initial) in
    let final = List.sort compare (if final = [] then automaton.final else final) in
    let rec repeat_in acc final' n = match n with
      | 0            -> single ()
      | 1            -> acc
      | i when i < 0 -> raise (Invalid_argument "Cannot repeat a negative amount of time")
      | _            -> let (acc, trans) = link ~state:final' ~state':initial acc automaton in
                        repeat_in acc (List.fold_left (fun fin f -> Hashtbl.find trans f::fin)
                                                      [] final) (n-1) in
    repeat_in automaton final n

  let chain ?(initial=[]) ?(final=[]) automaton =
    let initial = List.sort compare (if initial = [] then automaton.initial else initial) in
    let final = List.sort compare (if final = [] then automaton.final else final) in
    let initial_transitions = List.fold_left (fun x y -> x @ List.nth automaton.transitions y)
                                             [] initial in
    List.fold_left (fun a f -> List.fold_left (fun a' (t, i) -> add_transition a' f i t)
                                              a initial_transitions) automaton final

  let bypass ?(initial=[]) automaton =
    let initial = List.sort compare (if initial = [] then automaton.initial else initial) in
    add_final automaton initial

  let repeat_bypass automaton ?(initial=[]) ?(final=[]) ?(f=fun x -> x) n =
    let initial = List.sort compare (if initial = [] then automaton.initial else initial) in
    let final = List.sort compare (if final = [] then automaton.final else final) in
    let (acc, id) = add_state ~f automaton in
    let acc = List.fold_left (fun a i -> add_transition a i id Epsilon) acc initial in
    let rec repeat_bypass_in acc final' n = match n with
      | 0            -> single ()
      | 1            -> acc
      | i when i < 0 -> raise (Invalid_argument "Cannot repeat a negative amount of time")
      | _            -> let (acc, trans) = link ~state:final' ~state':initial acc automaton in
                        let acc = List.fold_left
                                    (fun a i -> add_transition a (Hashtbl.find trans i) id Epsilon)
                                    acc initial in
                        repeat_bypass_in acc
                                         (List.fold_left (fun fin f -> Hashtbl.find trans f::fin)
                                                         [] final) (n-1) in
    add_final (repeat_bypass_in acc final n) [id]

  let from_to automaton ?(initial=[]) ?(final=[]) start stop = match start, stop with
    | start, stop when start = stop -> repeat automaton start
    | start, stop when start > stop -> raise (Invalid_argument "start > stop")
    | start, stop                   -> link_ignore
                                         (link_ignore (repeat automaton ~initial ~final start)
                                                      (repeat_bypass ~initial ~final automaton
                                                                     (stop-start-1)))
                                         (bypass ~initial automaton)

  let loop ?(initial=[]) ?(final=[]) automaton =
    let initial = List.sort compare (if initial = [] then automaton.initial else initial) in
    let final = List.sort compare (if final = [] then automaton.final else final) in
    let automaton = { (chain ~initial ~final automaton) with final = automaton.initial @ automaton.final } in
    let (automaton, id) = add_state automaton in
    { (List.fold_left (fun a i -> add_transition a id i Epsilon) automaton initial) with initial = [id] }

  let rec safe_map f = function
    | hd::tl -> (try f hd::safe_map f tl with _ -> safe_map f tl)
    | []     -> []

  let next automaton (gstate, data) =
    let rec find_reachable = function
      | (Eta, s)::tl       -> (s, gstate.cursor, false)::find_reachable tl
      | (Epsilon, s)::tl   -> (s, gstate.cursor, true)::find_reachable tl
      | (Consume r, s)::tl -> if gstate.cursor >= List.length gstate.buffer ||
                                 not (E.mem (List.nth gstate.buffer gstate.cursor) r)
                              then find_reachable tl
                              else (s, gstate.cursor+1, true)::find_reachable tl
      | []                 -> [] in
    let rec update_steps id = function
      | { stop = None; start_state = None; _ } as s::tl
                                     -> { s with start_state = Some id }::(update_steps id tl)
      | { start_state = None; _ }::_ -> failwith "Corrupt stack"
      | hd::tl                       -> hd::(update_steps id tl)
      | []                           -> [] in
    let transitions = if gstate.id = -1 then (List.map (fun id -> (Epsilon, id)) automaton.initial)
                                        else (List.nth automaton.transitions gstate.id) in
    safe_map (fun (id, cursor, update) ->
                (List.nth automaton.states id)
                ({ gstate with id; cursor; steps = let (s, s') = gstate.steps in
                                                   if update then (s, update_steps id s')
                                                             else (s, s') }, data))
             (find_reachable transitions)

  let rec next_of_list automaton = function
    | hd::tl -> List.rev_append (next automaton hd) (next_of_list automaton tl)
    | []     -> []

  let is_final automaton id =
    List.mem id automaton.final

  let step automaton rstates =
    let next_rstates = next_of_list automaton rstates in
    (next_rstates, List.filter (fun (gstate, _) -> is_final automaton gstate.id) next_rstates)

  let check_with automaton rstates =
    let rec check_with_rec (rstates, rstates') =
      let rstates = List.sort_uniq compare rstates in
      match rstates, rstates' with
      | _::_, [] -> check_with_rec (step automaton rstates)
      | [], []   -> None
      | _, hd::_ -> Some hd in
    check_with_rec (rstates, [])

  let rec match_first_rec automaton ?(cursor=0) buffer (rstates_acc, rstates_acc') =
    let new_rstate = ({ id = -1; buffer; start = cursor; cursor; steps = ([], []) }, Data.empty) in
    let rstates = List.sort_uniq compare (if cursor > List.length buffer
                                          then rstates_acc else (new_rstate::rstates_acc)) in
    match rstates, rstates_acc' with
    | _::_, [] -> match_first_rec automaton ~cursor:(cursor+1) buffer (step automaton rstates)
    | [], []   -> None
    | _, hd::_ -> Some hd

  let match_first automaton buffer =
    match_first_rec automaton ~cursor:0 buffer ([], [])

  let check automaton buffer =
    match match_first automaton buffer with
      | Some _ -> true
      | None   -> false

  let reverse automaton =
    let rec reverse_transitions acc index = function
      | hd::tl -> reverse_transitions
                    (List.fold_left (fun acc (t, s) -> cons_to_nth acc s (t, index)) acc hd)
                    (index+1) tl
      | []     -> acc in
    let length = List.length automaton.states + 1 in
    let initial = List.map (fun i -> i+1) automaton.final in
    let final = List.map (fun i -> i+1) automaton.initial in
    let transitions = (List.map (fun i -> Epsilon, i) initial)::
                      reverse_transitions (Util.repeat (length-1) []) 1 automaton.transitions in
    let states = Util.repeat length (fun x -> x) in
    { initial = [0]; final; transitions; states }

  let best_match (gstate, data) (gstate', data') empty_is_none =
    let rec best_match_rec ?(left=true) ?(right=true) = function
      | ({ initial_state = s; _ } as hd)::tl,
        ({ initial_state = s'; _ } as hd')::tl' when s != s'
          -> (match if left then best_match_rec ~right:false (tl, hd'::tl') else None with
              | None -> if right then best_match_rec ~left:false (hd::tl, tl') else None
              | m    -> m)
      | ({ start = c; _ }:step)::_,
        ({ start = c'; _ }:step)::_ when c != c'     -> None
      | { greed = g; _ }::_,
        { greed = g'; _ }::_ when g != g'            -> None
      | { depth = d; _ }::_,
        { depth = d'; _ }::_ when d != d'            -> None
      | { start_state = Some i; _ }::_,
        { start_state = Some i'; _ }::_ when i != i' -> if i < i' then Some (gstate, data)
                                                                  else Some (gstate', data')
      | { start_state = None; _ }::_, _              -> failwith "Start state not set"
      | _, { start_state = None; _ }::_              -> failwith "Start state not set"
      | { stop = None; _ }::_, _                     -> failwith "Stop cursor not set"
      | _, { stop = None; _ }::_                     -> failwith "Stop cursor not set"
      | { greed = Greedy; stop = Some c; _ }::_,
        { stop = Some c'; _ }::_ when c != c'        -> if c < c' then Some (gstate', data')
                                                                  else Some (gstate, data)
      | { greed = Lazy; stop = Some c; _ }::_,
        { stop = Some c'; _ }::_ when c != c'        -> if c < c' then Some (gstate, data)
                                                                  else Some (gstate', data')
      | _::tl, _::tl'                                -> best_match_rec (tl, tl')
      | _, _                                         -> if empty_is_none then None
                                                                         else Some (gstate, data) in
    let (s, _) = gstate.steps in
    let (s', _) = gstate'.steps in
    best_match_rec (s, s')

  let rec match_one_rec automaton ?(cursor=0) buffer (rstates_acc, rstates_acc') =
    let rec eliminate_found acc = function
      | hd::hd'::tl -> (match best_match hd hd' true with
                        | Some best -> eliminate_found acc (best::tl)
                        | None      -> eliminate_found (hd'::acc) (hd::tl))
      | hd::[]      -> hd::eliminate_found [] acc
      | []          -> [] in
    let rec eliminate_current acc = function
      | hd::tl, hd'::tl' -> (match best_match hd hd' false with
                             | Some best when best = hd
                                      -> hd::eliminate_current acc (tl, (hd'::tl'))
                             | Some _ -> eliminate_current acc (tl, (hd'::tl'))
                             | None   -> eliminate_current (hd::acc) (tl, (hd'::tl')))
      | [], _::tl        -> eliminate_current [] (acc, tl)
      | l, []            -> l in
    let new_rstate = ({ id = -1; buffer; start = cursor; cursor; steps = ([], []) }, Data.empty) in
    let rstates = List.sort_uniq compare (if cursor > List.length buffer
                                          then rstates_acc else (new_rstate::rstates_acc)) in
    let found = eliminate_found [] rstates_acc' in
    let current = eliminate_current [] (rstates, found) in
    match current, found with
    | [], f -> f
    | c, f  -> let (c', f') = step automaton c in
               match_one_rec automaton ~cursor:(cursor+1) buffer (c', List.rev_append f f')

  let match_one automaton buffer =
    let rstates = List.sort (fun (g, _) (g', _) -> compare g.start g'.start)
                            (match_one_rec automaton ~cursor:0 buffer ([], [])) in
    if List.length rstates = 0 then None else
    let first_start = let (g, _) = List.nth rstates 0 in g.start in
    let rstates = List.filter (fun (g, _) -> g.start = first_start) rstates in
    Some (List.nth rstates 0)

  let insert_step step steps =
    let rec insert_step_rec steps = match step.depth, steps with
      | d, hd::tl when d = hd.depth -> step::hd::tl
      | d, hd::_ when d < hd.depth  -> failwith "Illegal step"
      | _, hd::tl                   -> hd::(insert_step_rec tl)
      | _, []                       -> [step] in
    List.rev (insert_step_rec (List.rev steps))

  let with_greed automaton depth greed =
    let open_marker (gstate, data) =
      let step = { greed; start = gstate.cursor; stop = None; initial_state = gstate.id;
                   start_state = None; depth } in
      ({ gstate with steps = let (s, s') = gstate.steps in (s, insert_step step s') }, data) in
    let close_marker (gstate, data) =
      let update_step steps =
        let rec update_step_rec (processed, current) = match processed, current, depth with
          | p, hd::tl, d when hd.stop = None
                         -> if hd.depth != d || hd.greed != hd.greed
                            then failwith "Corrupt stack"
                            else (p, { hd with stop = Some gstate.cursor }::tl)
          | p, hd::tl, _ -> let (p', c) = update_step_rec (p, tl) in (p', hd::c)
          | _, [], _     -> failwith "Corrupt stack" in
        let rec remove_repeated = function
          | hd::hd'::tl when hd.initial_state = hd'.initial_state
                   -> remove_repeated (hd::tl)
          | hd::tl -> hd::remove_repeated tl
          | []     -> [] in
        let (p, c) = update_step_rec steps in
        if depth = 0 then (p @ remove_repeated c, []) else (p, c) in
      ({ gstate with steps = update_step gstate.steps }, data) in
    link_ignore
      (link_ignore (link_ignore (of_transition ~f:open_marker Eta) (of_transition Epsilon))
                    automaton)
      (of_transition ~f:close_marker Eta)

  let initial automaton = automaton.initial
  let final automaton = automaton.final

  let order automaton = List.length automaton.states
  let size automaton = List.fold_left (fun i t -> i + List.length t) 0 automaton.transitions

  let assert_start ((gstate, _) as r) = assert (gstate.cursor = 0); r
  let assert_end ((gstate, _) as r) = assert (gstate.cursor = (List.length gstate.buffer)); r

  let restrict automaton =
    link_ignore (link_ignore (single ~f:assert_start ()) automaton) (single ~f:assert_end ())

  let restrict' automaton =
    link_ignore (link_ignore (of_transition ~f:assert_start Epsilon) automaton)
                (of_transition ~f:assert_end Epsilon)

end


module Make_simple (E : sig
                          type elt
                          type t
                          val mem : elt -> t -> bool
                        end) = Make (E) (Nothing)
