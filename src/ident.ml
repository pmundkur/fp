type t = {
  name  : string;
  stamp : int
}

let cur_stamp = ref 0

let make_ident name =
  incr cur_stamp;
  { name = name;
    stamp = !cur_stamp }

let name_of i = i.name

let compare i1 i2 =
  compare i1.stamp i2.stamp
