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

module Node:
  sig
    type 'a t

    val make_node: 'a -> bool -> 'a t

    val get_elem: 'a t -> 'a
    val get_children: 'a t -> 'a list
    val get_parents: 'a t -> 'a list
    val get_mark: 'a t -> bool

    val add_child: 'a t -> 'a -> 'a t
    val add_parent: 'a t -> 'a -> 'a t

    val is_root: 'a t -> bool
    val is_leaf: 'a t -> bool
  end

module Graph :
  functor (Elem : Hashtbl.HashedType) ->
    sig
      type t

      val init : unit -> t

      val add_link : t -> Elem.t -> Elem.t -> unit

      val get_children : t -> Elem.t -> Elem.t list
      val get_roots : t -> Elem.t list

      val iter : (Elem.t -> Elem.t Node.t -> unit) -> t -> unit

      exception Cycle of Elem.t list * Location.t
      val check_cycles : t -> Location.t -> unit
      val topo_sort : t -> Elem.t list
    end
