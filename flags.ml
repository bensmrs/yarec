type flag = [`CASELESS | `DOTALL | `MULTILINE]

let has : flag -> flag list -> bool = List.mem
