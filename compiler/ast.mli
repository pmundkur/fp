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

open Asttypes

type decl = {
  pdecl_desc: decl_desc;
  pdecl_loc: Location.t;
}

and decl_desc =
  | Pdecl_variant of decl_name * variant
  | Pdecl_format of decl_name * format

and variant = {
  pvariant_desc: variant_desc;
  pvariant_loc: Location.t;
}

and variant_desc = (exp * case_name * default) list

and format = {
  pformat_desc: format_desc;
  pformat_loc: Location.t;
}

and format_desc = field list

and field = {
  pfield_desc: field_desc;
  pfield_loc: Location.t;
}

and field_desc =
  | Pfield_name of field_name * field_type
  | Pfield_align of exp

and field_type = {
  pfield_type_desc: field_type_desc;
  pfield_type_loc: Location.t;
}

and field_type_desc =
  | Ptype_simple of type_exp * field_attrib list
  | Ptype_array of exp * format
  | Ptype_classify of exp * classify_case list
  | Ptype_label
  | Ptype_format of format_name

and field_attrib = {
  pfield_attrib_desc: field_attrib_desc;
  pfield_attrib_loc: Location.t;
}

and field_attrib_desc =
  | Pattrib_max of exp
  | Pattrib_min of exp
  | Pattrib_const of exp
  | Pattrib_default  of exp
  | Pattrib_variant_ref of decl_name
  | Pattrib_variant_inline of variant
  | Pattrib_value of value_case list

and value_case = {
  pvalue_case_desc: value_case_desc;
  pvalue_case_loc: Location.t;
}

and value_case_desc =
  | Pvalue_auto
  | Pvalue_default of exp
  | Pvalue_branch of branch_guard list * exp

and branch_guard = {
  pbranch_guard_desc: branch_guard_desc;
  pbranch_guard_loc: Location.t;
}

and branch_guard_desc = path * case_name

and type_exp = {
  ptype_exp_desc: type_exp_desc;
  ptype_exp_loc: Location.t;
}

and type_exp_desc =
  | Pbase of type_name
  | Pvector of type_name * exp

and classify_case = case_name * case_exp * format

and case_exp = {
  pcase_exp_desc: case_exp_desc;
  pcase_exp_loc: Location.t;
}

and case_exp_desc =
  | Pcase_const of exp
  | Pcase_range of exp * exp

and exp = {
  pexp_desc: exp_desc;
  pexp_loc: Location.t;
}

and exp_desc =
  | Pexp_unit
  | Pexp_var of path
  | Pexp_const_int of int
  | Pexp_const_int32 of Int32.t
  | Pexp_const_int64 of Int64.t
  | Pexp_apply of fun_name * exp list

and path =
  | Pfield of field_name
  | Ppath of field_name * path

val path_split: path -> (path (* prefix *) * field_name (* suffix *))
val path_decompose: path -> string list
val path_location_of: path -> Location.t

val pr_path: path -> string
val pr_decl: Format.formatter -> decl -> unit
