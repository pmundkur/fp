type t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position
}

let start_of l =
  l.loc_start

let end_of l =
  l.loc_end

let make_location s e =
  { loc_start = s;
    loc_end = e }

let dummy_loc =
  { loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos }

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
  if s.loc_start.Lexing.pos_cnum <= e.loc_start.Lexing.pos_cnum then
    { loc_start = s.loc_start;
      loc_end = e.loc_end }
  else
    { loc_start = e.loc_start;
      loc_end = s.loc_end }

let pr_loc oc loc =
  let line pos = pos.Lexing.pos_lnum in
  let col pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Printf.fprintf oc "%d.%d:%d.%d"
    (line loc.loc_start) (col loc.loc_start)
    (line loc.loc_end) (col loc.loc_end)

let pr_nloc oc ln =
  Printf.fprintf oc "%s(%a)" (node_of ln) pr_loc (location_of ln)

let pr_location oc loc =
  let file pos = pos.Lexing.pos_fname in
  let line pos = pos.Lexing.pos_lnum in
  let fl = file loc.loc_start in
  let sl = line loc.loc_start in
      Printf.fprintf oc "File \"%s\", line %d, characters %d-%d"
        fl sl (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
        (loc.loc_end.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
