type t

val start_of : t -> Lexing.position

val end_of : t -> Lexing.position

val make_location : Lexing.position -> Lexing.position -> t
type 'a located_node

val node_of : 'a located_node -> 'a

val location_of : 'a located_node -> t

val make_located_node : 'a -> t -> 'a located_node

