module Make (E : sig
                   type elt
                   type t
                   val mem : elt -> t -> bool
                 end) = struct

  type data = Stack of data Stack.t | Tag of string
  type state = { id: int;
                 data: (string, data) Hashtbl.t }
  type runtime_state = state * int
  type automaton_state = (runtime_state -> runtime_state)
  type transition_item = E.t option
  type t = { initial: int list;
             final: int list;
             states: automaton_state list;
             transitions: (transition_item * int) list list }
  module Int_set = Set.Make (Int)

  let empty = { initial = [];
                final = [];
                states = [];
                transitions = [] }

  let empty_state = { id = 0;
                      data = Hashtbl.create 4 }

  let rstate_of_id id = ({ empty_state with id }, 0)

  let next_available_state automaton = List.length automaton.states

  let id_of_state (i, _) = i
  let transformation_of_state (_, t) = t

  let add_state ?(f=fun x->x) ?(initial=false) ?(final=false) automaton =
    let id = next_available_state automaton in
    ({ initial = (if initial then id::automaton.initial else automaton.initial);
       final = (if final then id::automaton.final else automaton.final);
       states = automaton.states @ [f];
       transitions = automaton.transitions @ [[]] }, id)

  let single ?(f=fun x->x) () =
    let (a, id) = add_state ~f empty in { a with initial = [id]; final = [id] }

  let rec take l n = match l, n with
    | hd::tl, i when i > 0 -> hd::(take tl (i-1))
    | _, _                 -> []

  let rec drop l n = match l, n with
    | _::tl, i when i > 0 -> drop tl (i-1)
    | _, _                -> l

  let cons_to_nth l n e = (take l n) @ (e::List.nth l n)::(drop l (n+1))
  let append_to_nth l n e = (take l n) @ ((List.nth l n) @ e)::(drop l (n+1))

  let add_transition automaton start stop item =
    { automaton with transitions = cons_to_nth automaton.transitions start (item, stop) }

  let of_transition ?(f=fun x -> x) item =
    let (automaton, id) = add_state empty in
    let (automaton, id') = add_state ~f automaton in
    let automaton = add_transition automaton id id' item in
    { automaton with initial = [id]; final = [id'] }

  let remove_finals automaton finals =
    { automaton with final = Int_set.elements (Int_set.diff (Int_set.of_list automaton.final)
                                                            (Int_set.of_list finals)) }

  let add_finals automaton finals =
    { automaton with final = Int_set.elements (Int_set.union (Int_set.of_list automaton.final)
                                                             (Int_set.of_list finals)) }

  let link ?(state=([])) ?(state'=([])) automaton automaton' =
    let state = List.sort compare (if state = [] then automaton.final else state) in
    let state' = List.sort compare (if state' = [] then automaton'.initial else state') in
    let trans = Hashtbl.create (next_available_state automaton') in
    ignore (List.map (fun s -> List.map (fun s' -> Hashtbl.add trans s' s) state') state);
    let rec merge_states automaton ?(n=0) states = match n, states with
      | id, _::tl when List.mem id state'
           -> merge_states automaton ~n:(n+1) tl
      | id, f::tl
           -> let id' = next_available_state automaton in
                        (Hashtbl.add trans id id';
                         merge_states { automaton with states = automaton.states @ [f];
                                                       transitions = automaton.transitions @ [[]] }
                                      ~n:(n+1) tl)
      | _, [] -> automaton in
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
    (add_finals (remove_finals automaton'' automaton.final)
                (List.fold_left (fun f n -> f @ (Hashtbl.find_all trans n)) [] automaton'.final),
     trans)

  let link_ignore ?(state=([])) ?(state'=([])) automaton automaton' =
    let (a, _) = link ~state ~state' automaton automaton' in a

  let get_ends automaton initial final =
    let initial = if initial = -1 then match automaton.initial with
        | hd::[] -> hd
        | _      -> raise (Invalid_argument "The automaton must have exactly one initial state")
      else initial in
    let final = if final = -1 then match automaton.final with
        | hd::[] -> hd
        | _      -> raise (Invalid_argument "The automaton must have exactly one final state")
      else final in
    (initial, final)

  let repeat automaton ?(initial=(-1)) ?(final=(-1)) n =
    let (initial, final) = get_ends automaton initial final in
    let rec repeat_in acc final' n = match n with
      | 0            -> single ()
      | 1            -> acc
      | i when i < 0 -> raise (Invalid_argument "Cannot repeat a negative amount of time")
      | _            -> let (acc, trans) = link ~state:[final'] ~state':[initial] acc automaton in
                        repeat_in acc (Hashtbl.find trans final) (n-1) in
    repeat_in automaton final n

  let chain ?(initial=(-1)) ?(final=(-1)) automaton =
    let (initial, final) = get_ends automaton initial final in
    add_transition automaton final initial None

  let bypass automaton = add_finals automaton automaton.initial

  let repeat_bypass automaton ?(initial=(-1)) ?(final=(-1)) ?(f=fun x -> x) n =
    let (initial, final) = get_ends automaton initial final in
    let (acc, id) = add_state ~f automaton in
    let acc = (add_transition acc initial id None) in
    let rec repeat_bypass_in acc final' n = match n with
      | 0            -> single ()
      | 1            -> acc
      | i when i < 0 -> raise (Invalid_argument "Cannot repeat a negative amount of time")
      | _            -> let (acc, trans) = link ~state:[final'] ~state':[initial] acc automaton in
                        let acc = add_transition acc (Hashtbl.find trans initial) id None in
                        repeat_bypass_in acc (Hashtbl.find trans final) (n-1) in
    add_finals (repeat_bypass_in acc final n) [id]

  let loop ?(initial=(-1)) ?(final=(-1)) automaton =
    let (initial, final) = get_ends automaton initial final in
    { (chain ~initial ~final automaton) with final = automaton.initial }

  let add_state_from ?(f=fun x -> x) automaton state item =
    let (automaton, id) = add_state ~f automaton in
    (add_transition automaton state id item, id)

  let rec add_states_from ?(f=[]) automaton state items = match f, items with
    | _, []            -> (automaton, state)
    | [], hd::tl       -> let (automaton, state) = add_state_from automaton state hd in
                          add_states_from automaton state tl
    | hd::tl, hd'::tl' -> let (automaton, state) = add_state_from ~f:hd automaton state hd' in
                          add_states_from ~f:tl automaton state tl'

  let next automaton (state, cursor) buf =
    let rec find_reachable = function
      | (None, s)::tl   -> (s, cursor)::find_reachable tl
      | (Some r, s)::tl -> if cursor >= List.length buf || not (E.mem (List.nth buf cursor) r)
                           then find_reachable tl
                           else (s, cursor+1)::find_reachable tl
      | []              -> [] in
    let transitions = List.nth automaton.transitions state.id in
    List.map (fun (id, c) -> (List.nth automaton.states id) ({ state with id }, c))
             (find_reachable transitions)

  let rec next_of_list automaton rstates buf = match rstates with
    | hd::tl -> next automaton hd buf @ next_of_list automaton tl buf
    | []     -> []

  let is_final automaton (state, cursor) buf =
    cursor = List.length buf && List.mem state.id automaton.final

  let step automaton rstates buf =
    let next_rstates = next_of_list automaton rstates buf in
    (next_rstates, List.filter (fun rs -> is_final automaton rs buf) next_rstates)

  let check automaton buf =
    if is_final automaton (rstate_of_id 0) buf then true else
    let rec check_rec = function
      | hd::tl, [] -> check_rec (step automaton (hd::tl) buf)
      | [], []     -> false
      | _, _       -> true in
    let rstates = List.map rstate_of_id automaton.initial in
    check_rec (rstates, [])
end
