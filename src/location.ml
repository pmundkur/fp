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

let symbol_rloc () =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos () }

let rhs_loc i =
  { loc_start = Parsing.rhs_start_pos i;
    loc_end = Parsing.rhs_end_pos i }

let span s e =
  if s.loc_start.pos_cnum <= e.loc_start.pos_cnum then
    { loc_start = s.loc_start;
      loc_end = e.loc_end }
  else
    { loc_start = e.loc_start;
      loc_end = s.loc_end }
