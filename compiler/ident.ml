(**************************************************************************)
(*  Copyright 2009          Prashanth Mundkur.                            *)
(*  Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>          *)
(*                                                                        *)
(*  This file is part of FormatCompiler.                                  *)
(*                                                                        *)
(*  FormatCompiler is free software: you can redistribute it and/or       *)
(*  modify it under the terms of the GNU Affero General Public            *)
(*  License as published by the Free Software Foundation, either          *)
(*  version 3 of the License, or (at your option) any later version.      *)
(*                                                                        *)
(*  FormatCompiler is distributed in the hope that it will be useful,     *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with FormatCompiler.  If not, see                       *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

type t =
    { name: string;
      stamp: int;
      loc: Location.t }

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

let hash i = i.stamp


type 'a env = (t * 'a) list

let empty_env = []

let add id info env =
  (id, info) :: env

let rec replace id info = function
  | [] ->
      raise Not_found
  | ((id', _) as ent) :: tl ->
      if compare id id' = 0 then
        (id, info) :: tl
      else
        ent :: (replace id info tl)

let put id info env =
  try
    replace id info env
  with
    | Not_found ->
        add id info env

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

let rec map f env =
  match env with
    | [] -> []
    | (i, info) :: tl ->
        (i, f i info) :: map f tl

let pr_ident_name id =
  id.name
