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

type syntax_error =
  | Illegal_character of char
  | Unmatched_comment
  | Unterminated_comment

exception Syntax_error of syntax_error * Lexing.position

type parse_error =
  | Unknown_operator of string
  | Unknown_field_attribute of string

exception Parsing_error of parse_error * Location.t

type default = bool

type decl_name = string Location.located_node

type field_name = string Location.located_node

type case_name = string Location.located_node

type fun_name = string Location.located_node

type type_name = string Location.located_node

type label_name = string Location.located_node

type format_name = string Location.located_node
