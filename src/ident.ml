type t = {
  name: string;
  stamp: int;
  loc: Location.t
}

let cur_stamp = ref 0

let make_from_string name loc =
  incr cur_stamp;
  { name = name;
    stamp = !cur_stamp;
    loc = loc }

let make_from_node n =
  make_from_string (Location.node_of n) (Location.location_of n)

let name_of i = i.name

let location_of i = i.loc

let compare i1 i2 =
  compare i1.stamp i2.stamp
