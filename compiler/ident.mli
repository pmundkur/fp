(**************************************************************************)
(*  Copyright 2009          Prashanth Mundkur.                            *)
(*  Author  Prashanth Mundkur <prashanth.mundkur@gmail.com>               *)
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

type t

val from_string: string -> Location.t -> t

val from_node: string Location.located_node -> t

val name_of: t -> string

val location_of: t -> Location.t

val compare: t -> t -> int

type 'a env

val empty_env: 'a env

val add: t -> 'a -> 'a env -> 'a env

val exists: (t -> 'a -> bool) -> 'a env -> bool

val assoc_by_name: 'a env -> string -> (t * 'a) option

val assoc_by_id: 'a env -> t -> 'a option

val extend: 'a env -> 'a env -> 'a env

val fold: (t -> 'a -> 'b -> 'b) -> 'a env -> 'b -> 'b

val iter: (t -> 'a -> unit) -> 'a env -> unit

val pr_ident_name: t -> string

