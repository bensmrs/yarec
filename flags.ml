type flag = [`DOTALL]

let has : flag -> flag list -> bool = List.mem
