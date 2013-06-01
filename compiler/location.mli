(**************************************************************************)
(*  Copyright 2009-2013       Prashanth Mundkur.                          *)
(*  Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>          *)
(*                                                                        *)
(*  This file is part of FormatCompiler.                                  *)
(*                                                                        *)
(*  FormatCompiler is free software: you can redistribute it and/or       *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation, either          *)
(*  version 3 of the License, or (at your option) any later version.      *)
(*                                                                        *)
(*  Alternatively, this software may be distributed, used, and modified   *)
(*  under the terms of the BSD license.                                   *)
(*                                                                        *)
(*  FormatCompiler is distributed in the hope that it will be useful,     *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(**************************************************************************)

type t

val start_of: t -> Lexing.position

val end_of: t -> Lexing.position

val make_location: Lexing.position -> Lexing.position -> t

val dummy_loc: t

type 'a located_node

val node_of: 'a located_node -> 'a

val location_of: 'a located_node -> t

val make_located_node: 'a -> t -> 'a located_node

val symbol_rloc: unit -> t

val rhs_loc: int -> t

val span: t -> t -> t

val pr_line_info: t -> string
val pr_location: t -> string

