module Make (E : sig
                   type elt
                   type t
                   val mem : elt -> t -> bool
                 end) = struct

  type data = Stack of data Stack.t | String of string
  type state = { id: int;
                 data: (string, data) Hashtbl.t }
  type automaton_state = int * (state -> state)
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

  let next_available_state automaton = List.length automaton.states

  let id_of_state (i, _) = i
  let transformation_of_state (_, t) = t

  let add_state ?(f=fun x->x) ?(initial=false) ?(final=false) automaton =
    let id = next_available_state automaton in
    let state = (id, f) in
    ({ initial = (if initial then id::automaton.initial else automaton.initial);
       final = (if final then id::automaton.final else automaton.final);
       states = automaton.states @ [state];
       transitions = automaton.transitions @ [[]] }, id)

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
    let (automaton, id') = add_state ~f:f automaton in
    let automaton = add_transition automaton id id' item in
    { automaton with initial = [id]; final = [id'] }

  let remove_finals automaton finals =
    { automaton with final = Int_set.elements (Int_set.diff (Int_set.of_list automaton.final)
                                                            (Int_set.of_list finals)) }

  let add_finals automaton finals =
    { automaton with final = Int_set.elements (Int_set.union (Int_set.of_list automaton.final)
                                                             (Int_set.of_list finals)) }

  let link ?(state=(-1)) ?(state'=(-1)) automaton automaton' =
    let state = if state = -1 then match automaton.final with
        | hd::[] -> hd
        | _      -> raise (Invalid_argument "The first automaton must have exactly one final state")
      else state in
    let state' = if state' = -1 then match automaton'.initial with
        | hd::[] -> hd
        | _      -> raise (Invalid_argument
                             "The second automaton must have exactly one initial state")
      else state' in
    let trans = Hashtbl.create (next_available_state automaton') in
    Hashtbl.add trans state' state;
    let rec merge_states automaton = function
      | (id, _)::tl when id = state'
           -> merge_states automaton tl
      | (id, f)::tl
           -> let id' = next_available_state automaton in
                        (Hashtbl.add trans id id';
                         merge_states { automaton with states = automaton.states @ [(id', f)];
                                                       transitions = automaton.transitions @ [[]] }
                                      tl)
      | [] -> automaton in
    let rec merge_transitions automaton ?(n=0) transitions =
      let rec update_transition = function
        | (item, id)::tl -> (item, Hashtbl.find trans id)::(update_transition tl)
        | []             -> [] in
      match transitions with
        | hd::tl -> let transition = update_transition hd in
                    let transitions' = append_to_nth automaton.transitions (Hashtbl.find trans n)
                                                     transition in
                    merge_transitions { automaton with transitions = transitions' } ~n:(n+1) tl
        | []     -> automaton in
    let automaton'' = merge_transitions (merge_states automaton automaton'.states)
                                        automaton'.transitions in
    (add_finals (remove_finals automaton'' automaton.final)
                (List.map (fun x -> Hashtbl.find trans x) automaton'.final), trans)

  let link_ignore ?(state=(-1)) ?(state'=(-1)) automaton automaton' =
    let (a, _) = link ~state:state ~state':state' automaton automaton' in a

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
      | 1            -> acc
      | i when i < 1 -> raise (Invalid_argument "Cannot repeat fewer than once")
      | _            -> let (acc, _) = link ~state:final' ~state':initial acc automaton in
                        repeat_in acc final' (n-1) in
    repeat_in automaton final n

  let chain ?(initial=(-1)) ?(final=(-1)) automaton =
    let (initial, final) = get_ends automaton initial final in
    add_transition automaton final initial None

  let bypass ?(initial=(-1)) ?(final=(-1)) automaton =
    let (initial, _) = get_ends automaton initial final in
    add_finals automaton [initial]

  let repeat_bypass automaton ?(initial=(-1)) ?(final=(-1)) ?(f=fun x -> x) n =
    let (initial, final) = get_ends automaton initial final in
    let (acc, id) = add_state ~f:f automaton in
    let acc = add_transition acc initial id None in
    let rec repeat_bypass_in acc final' n = match n with
      | 1            -> acc
      | i when i < 1 -> raise (Invalid_argument "Cannot repeat fewer than once")
      | _            -> let (acc, trans) = link ~state: final' ~state': initial acc automaton in
                        let acc = add_transition acc (Hashtbl.find trans initial) id None in
                        repeat_bypass_in acc final' (n-1) in
    repeat_bypass_in acc final n

  let loop ?(initial=(-1)) ?(final=(-1)) automaton =
    let (initial, final) = get_ends automaton initial final in
    { (chain ~initial:initial ~final:final automaton) with final = automaton.initial }

  let add_state_from ?(f=fun x -> x) automaton state item =
    let (automaton, id) = add_state ~f:f automaton in
    (add_transition automaton state id item, id)

  let rec add_states_from ?(f=[]) automaton state items = match f, items with
    | _, []            -> (automaton, state)
    | [], hd::tl       -> let (automaton, state) = add_state_from automaton state hd in
                          add_states_from automaton state tl
    | hd::tl, hd'::tl' -> let (automaton, state) = add_state_from ~f:hd automaton state hd' in
                          add_states_from ~f:tl automaton state tl'

  let reachable_states automaton state symbol =
    let rec find_reachable symbol = function
      | (Some r, s)::tl when E.mem symbol r -> s::find_reachable symbol tl
      | _::tl                               -> find_reachable symbol tl
      | []                                  -> [] in
    let transitions = List.nth automaton.transitions state.id in
    List.map (fun i -> List.nth automaton.states i)
             (find_reachable symbol (transitions))

  let next automaton state symbol =
    List.map (fun x -> { (transformation_of_state x state) with id = id_of_state x })
             (reachable_states automaton state symbol)

  let rec next_of_list automaton states symbol = match states with
    | hd::tl -> next automaton hd symbol @ next_of_list automaton tl symbol
    | []     -> []
end
