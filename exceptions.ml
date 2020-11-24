exception Illegal_state of { desc: string; loc: Location.t }

let illegal_state msg loc =
  raise (Illegal_state { desc = msg; loc = loc })
