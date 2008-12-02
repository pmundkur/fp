
type t

val from_string: string -> Location.t -> t

val from_node: string Location.located_node -> t

val name_of: t -> string

val location_of: t -> Location.t

val compare: t -> t -> int

type 'a env

val empty_env: 'a env

val add: t -> 'a -> 'a env -> 'a env

val assoc_by_name: 'a env -> string -> (t * 'a) option

val assoc_by_id: 'a env -> t -> 'a option

val extend: 'a env -> 'a env -> 'a env

val pr_ident_name: t -> string

