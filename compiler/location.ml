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

let pr_line_info loc =
  Printf.sprintf "line %d, characters %d-%d"
    loc.loc_start.Lexing.pos_lnum
    (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
    (loc.loc_end.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)

let pr_location loc =
    Printf.sprintf "File \"%s\", %s"
      loc.loc_start.Lexing.pos_fname (pr_line_info loc)
