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

module PathMap = Map.Make (struct type t = string list let compare = compare end)

type t = {
  (* Functions and types are currently predefined only. *)
  functions: Types.function_info Ident.env;
  types: Types.type_info Ident.env;

  (* Variants and formats are defined at the top-level. *)
  variants: Types.variant_info Ident.env;
  formats: Types.format_info Ident.env;

  (* Fields need a dynamic stacked environment. *)
  fields: Types.field_type Ident.env;

  (* Map from paths to structs *)
  path_map: (Types.path * Types.struct_type) PathMap.t;
  paths: (Types.path * Asttypes.case_name * Types.struct_type) list;

  (* List of branch fields used for classification *)
  branch_fields: (Ident.t * Location.t) list;
}

let new_env () = {
  types             = Ident.empty_env;
  functions         = Ident.empty_env;
  variants          = Ident.empty_env;
  formats           = Ident.empty_env;
  fields            = Ident.empty_env;
  path_map          = PathMap.empty;
  paths             = [];
  branch_fields     = [];
}

let all_fields_by_id = ref Ident.empty_env

let lookup_field_by_name t n =
  Ident.assoc_by_name t.fields n

let lookup_field_by_id t i =
  Ident.assoc_by_id t.fields i

let lookup_variant_by_name t n =
  Ident.assoc_by_name t.variants n

let lookup_variant_by_id t i =
  Ident.assoc_by_id t.variants i

let lookup_format_by_name t n =
  Ident.assoc_by_name t.formats n


let lookup_format_by_id t i =
  Ident.assoc_by_id t.formats i

let lookup_function_by_name t n =
  Ident.assoc_by_name t.functions n

let lookup_function_by_id t i =
  Ident.assoc_by_id t.functions i

let lookup_type_by_name t n =
  Ident.assoc_by_name t.types n

let lookup_type_by_id t i =
  Ident.assoc_by_id t.types i


let global_lookup_field_by_id t i =
  Ident.assoc_by_id !all_fields_by_id i

let get_formats t =
  t.formats

let add_type tid tinfo env =
  { env with
      types = Ident.add tid tinfo env.types;
  }

let add_function fid finfo env =
  { env with
      functions = Ident.add fid finfo env.functions;
  }

let add_variant_def vid vinfo env =
  { env with
      variants = Ident.add vid vinfo env.variants;
  }

let add_field fid finfo env =
  all_fields_by_id := Ident.add fid finfo !all_fields_by_id;
  { env with
      fields = Ident.add fid finfo env.fields;
  }

let add_format_def fid finfo env =
  { env with
      formats = Ident.add fid finfo env.formats;
  }


let lookup_path t p =
  try Some (PathMap.find (Ast.path_decompose p) t.path_map)
  with Not_found -> None

let add_path p cn s env =
  { env with
      path_map = PathMap.add (Types.path_decompose p) (p, s) env.path_map;
      paths = (p, cn, s) :: env.paths;
  }

let get_paths env =
  List.rev env.paths

let extract_field_env env =
  env.fields

let add_branch_field fid loc env =
  { env with
      branch_fields = (fid, loc) :: env.branch_fields;
  }

let find_branch_field t fid =
  try Some (List.assoc fid t.branch_fields)
  with Not_found -> None

let clone env =
  { env with
      fields = Ident.empty_env;
      path_map = PathMap.empty;
      paths = [];
      branch_fields = [];
  }
