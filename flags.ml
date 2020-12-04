type t = [`CASELESS | `DOTALL | `MULTILINE]

let has : t -> t list -> bool = List.mem
