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

  let next_available_state automaton = List.length automaton.states

  let id_of_state (i, _) = i
  let transformation_of_state (i, t) = t

  let add_state ?(f=fun x -> x) ?(initial=false) ?(final=false) automaton =
    let id = next_available_state automaton in
    let state = (id, f) in
    let transition = [] in
    ({ initial = (if initial then id::automaton.initial else automaton.initial);
       final = (if final then id::automaton.final else automaton.final);
       states = automaton.states @ [state];
       transitions = automaton.transitions @ [transition] }, id)

  let add_transition automaton item start stop =
    let rec take l n = match l, n with
      | hd::tl, i when i > 0 -> hd::(take tl (i-1))
      | hd::tl, _            -> []
      | [], _                -> [] in
    let rec drop l n = match l, n with
      | hd::tl, i when i > 0 -> take tl (i-1)
      | hd::tl, _            -> tl
      | [], _                -> [] in
    { automaton with transitions = (take automaton.transitions (start-1))@
                                   ((item, stop)::List.nth automaton.transitions start)::
                                   (drop automaton.transitions start) }

  let reachable_states automaton state symbol =
    let rec find_reachable symbol = function
      | (Some r, s)::tl when E.mem symbol r -> s::find_reachable symbol tl
      | (_, s)::tl                          -> find_reachable symbol tl
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

  let empty = { initial = [];
                final = [];
                states = [];
                transitions = [] }
end
