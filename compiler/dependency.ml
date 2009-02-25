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

(* The section below performs generation-facet checks on auto-computed
   fields.

   Fields that can be auto-computed are:
   . fields used for classification branches (i.e. "branching fields")
   . fields that occur in vector or array length expressions, provided
     the length expression is a single variable.

   Fields that have a value attribute share with auto-computed fields
   the fact that both are omitted from the generation API.

   The checks should ensure that if a value attribute is specified for
   such fields, they are consistent with their parsing facet
   declaration.  Also, auto-computed fields should not have multiple
   ways of being autocomputed; if such multiple ways exist, they
   revert to being specified fields, and an optional warning should be
   emitted indicating potential inconsistencies between the parsing
   and generation facets.

   The checks for fields involved in vector and array length
   expressions are performed below.  An additional check performed is
   to disallow variant fields from being used as array/vector lengths.

   Also, checks are performed to ensure that expressions for values
   attributes do not have dependencies cycles.
*)

open Types

(* Paths are from one field to another, possibly nested, field within
   the same struct.  The nesting may be within in an array or a
   classify block; in this case, the paths are relative to the outer
   field.
*)
type dpath =
    (* field at same level of nesting *)
  | Field of Ident.t
    (* field within a branch of a classify block *)
  | In_classify of struct_type * Ident.t * Asttypes.case_name * dpath
    (* field within an array block *)
  | In_array of Ident.t * dpath
    (* field within a struct block *)
  | In_struct of Ident.t * dpath

(* A context frame.  These form entries in a context stack. *)
type frame =
  | Classify_block of struct_type * Ident.t * Asttypes.case_name
  | Array_block of Ident.t
  | Struct_block of Ident.t

let rec make_dpath id = function
  | [] ->
      Field id
  | Classify_block (st, cid, cn) :: tl ->
      In_classify (st, cid, cn, make_dpath id tl)
  | Array_block aid :: tl ->
      In_array (aid, make_dpath id tl)
  | Struct_block sid :: tl ->
      In_struct (sid, make_dpath id tl)

type dependency =
    { attribs: field_attribs;
      (* controlled by usage in remote fields *)
      length: dpath list;
      branch: dpath list;
      length_expr: dpath list;
    }

let null_dependency =
  { attribs = null_field_attribs;
    length = [];
    branch = [];
    length_expr = []
 }

let generate_depinfo fmt =
  let fenv = ident_map fmt in
  let lookup_dep deps id =
    match Ident.assoc_by_id deps id with
      | Some d -> d
      | None ->
          (match Ident.assoc_by_id fenv id with
             | None ->
                 raise (Failure ("generate_depinfo: failure to look up "
                                 ^ (Ident.name_of id)))
             | Some (ft, fas) ->
                 { null_dependency with attribs = fas }) in
  let store_dep tid dep deps =
    Ident.put tid dep deps in
  let rec process_length_expr deps ctxt lid e =
    let dpath = make_dpath lid ctxt
    in
      match e.exp_desc with
        | Texp_var p ->
            let tid = path_tail_ident p in
            let dep = lookup_dep deps tid in
            let dep = { dep with length = dpath :: dep.length }
            in
              store_dep tid dep deps
        | _ ->
            let tidl = List.map path_tail_ident (vars_of_exp e)
            in
              List.fold_left
                (fun deps tid ->
                   let dep = lookup_dep deps tid in
                   let dep = { dep with length_expr = dpath :: dep.length_expr }
                   in
                     store_dep tid dep deps)
                deps tidl in
  let rec process_field deps (ctxt, cst) id (ft, fas) =
    (* Update deps based on the field attributes *)
    let dep = lookup_dep deps id in
    let deps = store_dep id dep deps
    in
      (* Update deps based on the field type *)
      match ft with
        | Ttype_base (Tbase_primitive _) ->
            deps
        | Ttype_base (Tbase_vector (_, e)) ->
            process_length_expr deps ctxt id e
        | Ttype_struct st ->
            process_struct deps (Struct_block id :: ctxt) st
        | Ttype_map (bid, mt) ->
            let dep = lookup_dep deps bid in
            let dep = { dep with branch = (make_dpath id ctxt) :: dep.branch } in
            let deps = store_dep bid dep deps
            in
              StringMap.fold
                (fun _ (cn, _, st) deps ->
                   process_struct deps (Classify_block (cst, id, cn) :: ctxt) st)
                mt.map_type_desc deps
        | Ttype_array (e, st) ->
            let deps = process_length_expr deps ctxt id e
            in
              process_struct deps (Array_block id :: ctxt) st
        | Ttype_label | Ttype_format _ ->
            deps
  and process_struct deps ctxt st =
    Ident.fold
      (fun f fi deps -> process_field deps (ctxt, st) f fi)
      st.fields deps
  in
    process_struct Ident.empty_env [] fmt
