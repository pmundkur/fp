open Lexing

type t = {
  loc_start : position;
  loc_end : position
}

let start_of l =
  l.loc_start

let end_of l =
  l.loc_end

let make_location s e =
  { loc_start = s;
    loc_end = e }

type 'a located_node =
    { node : 'a;
      location : t }

let node_of loc_node =
  loc_node.node

let location_of loc_node =
  loc_node.location

let make_located_node node loc =
  { node = node;
    location = loc }

