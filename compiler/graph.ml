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

module Node = struct
  type 'a t = {
    elem: 'a;
    mark: bool;
    children: 'a list;
    parents: 'a list
  }

  let make_node elem mark = {
    elem     = elem;
    mark     = mark;
    children = [];
    parents  = [];
  }

  let get_elem v = v.elem
  let get_children v = v.children
  let get_parents v = v.parents
  let get_mark v = v.mark

  let add_child v c =
    if List.mem c v.children then v
    else { v with children = c :: v.children }
  let add_parent v p =
    if List.mem p v.parents then v
    else { v with parents = p :: v.parents }

  let is_root v = v.parents = []
  let is_leaf v = v.children = []
end

module Graph =
  functor (Elem: Hashtbl.HashedType) -> struct
    module H = Hashtbl.Make (Elem)
    type t = (Elem.t Node.t) H.t

    let init () = H.create 10

    let add_link (g : t) (p : Elem.t) (c : Elem.t) =
      let pn = try H.find g p with Not_found -> Node.make_node p true in
      let cn = try H.find g c with Not_found -> Node.make_node c false in
      let pn' = Node.add_child pn c in
      let cn' = Node.add_parent cn p in
        H.replace g p pn';
        H.replace g c cn'

    let get_children g v =
      Node.get_children (H.find g v)

    let get_roots g =
      H.fold (fun v n acc ->
                if Node.is_root n then (Node.get_elem n) :: acc
                else acc
             ) g []

    let iter = H.iter

    type order =
      | Pre_order
      | Post_order

    type node_select =
      | Node_select_all
      | Node_select_marked

    let dfs_traverse g f order node_select root =
      let rec traverse stack elem =
        let node = H.find g elem in
        let selected =
          match node_select with
            | Node_select_all -> true
            | Node_select_marked -> node.Node.mark
        in
          match order with
            | Pre_order ->
                if selected then f stack elem;
                List.iter (traverse (elem :: stack)) (Node.get_children node)
            | Post_order ->
                List.iter (traverse (elem :: stack)) (Node.get_children node);
                if selected then f stack elem
      in
        traverse [] root

    exception Cycle of Elem.t list * Location.t

    let check_cycles g loc =
      let roots = get_roots g in
      let checker stack elem =
        if List.mem elem stack then
          raise (Cycle ((elem :: stack), loc))
      in
        List.iter (dfs_traverse g checker Pre_order Node_select_all) roots

    (* NOTE: The topo sorter assumes that the graph has been checked
       for cycles with the above function. *)
    let topo_sort g =
      let roots = get_roots g in
      let order = ref [] in
      let sorter stack elem =
        if not (List.mem elem !order) then
          order := elem :: !order
      in
        List.iter (dfs_traverse g sorter Post_order Node_select_marked) roots;
        !order
  end
