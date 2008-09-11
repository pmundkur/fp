
type t

val make_from_string: string -> Location.t -> t

val make_from_node: string Location.located_node -> t

val name_of: t -> string

val location_of: t -> Location.t

val compare: t -> t -> int



