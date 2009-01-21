type t = {
  name: string;
  stamp: int;
  loc: Location.t
}

let cur_stamp = ref 0

let from_string name loc =
  incr cur_stamp;
  { name = name;
    stamp = !cur_stamp;
    loc = loc }

let from_node n =
  from_string (Location.node_of n) (Location.location_of n)

let name_of i = i.name

let location_of i = i.loc

let compare i1 i2 =
  compare i1.stamp i2.stamp


type 'a env = (t * 'a) list

let empty_env = []

let add id info env =
  (id, info) :: env

let exists f env =
  let rec ex = function
    | [] ->
        false
    | (i, info) :: tl ->
        if f i info then true else ex tl
  in
    ex env

let assoc_match match_fn ret_fn env n =
  let rec assc = function
    | [] ->
        None
    | (i, info) :: tl ->
        if match_fn i n then ret_fn i info
        else assc tl
  in
    assc env

let assoc_by_name env n =
  assoc_match
    (fun i n -> name_of i = n)
    (fun i info -> Some (i, info))
    env n

let assoc_by_id env i =
  assoc_match
    (fun i n -> compare i n = 0)
    (fun i info -> Some info)
    env i

let extend first second =
  first @ second

let rec fold f env init =
  match env with
    | [] -> init
    | (i, info) :: tl ->
        f i info (fold f tl init)

let rec iter f env =
  match env with
    | [] -> ()
    | (i, info) :: tl ->
        f i info;
        iter f tl

let pr_ident_name id =
  id.name
