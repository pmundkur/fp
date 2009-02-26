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

let rec dpath_tail_ident = function
  | Field id -> id
  | In_classify (_, _, _, dp)
  | In_array (_, dp)
  | In_struct (_, dp) -> dpath_tail_ident dp

let is_local_dpath = function
  | Field _ -> true
  | In_classify _ | In_array _ | In_struct _ -> false

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
    { field_type: field_type;
      field_attribs: field_attribs;
      (* controlled by usage in remote fields *)
      length_of: dpath list;
      branch_of: dpath list;
      in_length_of: dpath list;
      (* analysis result *)
      mutable autocompute: bool
    }

let make_dep ft fas =
  { field_type = ft;
    field_attribs = fas;
    length_of = [];
    branch_of = [];
    in_length_of = [];
    autocompute = false
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
                 make_dep ft fas) in
  let store_dep tid dep deps =
    Ident.put tid dep deps in
  let rec process_length_expr deps ctxt lid e =
    let dpath = make_dpath lid ctxt
    in
      match e.exp_desc with
        | Texp_var p ->
            let tid = path_tail_ident p in
            let dep = lookup_dep deps tid in
            let dep = { dep with length_of = dpath :: dep.length_of }
            in
              store_dep tid dep deps
        | _ ->
            let tidl = List.map path_tail_ident (vars_of_exp e)
            in
              List.fold_left
                (fun deps tid ->
                   let dep = lookup_dep deps tid in
                   let dep = { dep with in_length_of = dpath :: dep.in_length_of }
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
            let dep = { dep with
                          branch_of = (make_dpath id ctxt) :: dep.branch_of } in
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

type dep_warning =
  | Multiple_length_use of (* field *) Ident.t * (* lengths *) Ident.t * Ident.t
  | Non_local_length_use of (* field *) Ident.t * (* length *) Ident.t
  | Multiple_branch_use of (* field *) Ident.t * (* branches *) Ident.t * Ident.t
  | Branch_and_length_use of (* field *) Ident.t * (* branch *) Ident.t * (* length *) Ident.t
  | Const_as_length_use of (* const *) Ident.t * (* length *) Ident.t
  | Variant_as_length_use of (* variant *) Ident.t * (* length *) Ident.t

let warnmsg = function
  | Multiple_length_use (fid, l1, l2) ->
      Printf.sprintf "%s: field %s is used multiple times in a length (for fields %s and %s)"
        (Location.pr_location (Ident.location_of fid))
        (Ident.pr_ident_name fid) (Ident.pr_ident_name l1) (Ident.pr_ident_name l2)
  | Non_local_length_use (fid, l) ->
      Printf.sprintf "%s: use of field %s in the length of a non-local field %s requires a value specification to be auto-computed"
        (Location.pr_location (Ident.location_of fid))
        (Ident.pr_ident_name fid) (Ident.pr_ident_name l)
  | Multiple_branch_use (fid, br1, br2) ->
      Printf.sprintf "%s: field %s is used multiple times as a classify brancher (for fields %s and %s)"
        (Location.pr_location (Ident.location_of fid))
        (Ident.pr_ident_name fid) (Ident.pr_ident_name br1) (Ident.pr_ident_name br2)
  | Branch_and_length_use (fid, br, l) ->
      Printf.sprintf "%s: field %s is used as a classify brancher for field %s and in the length of field %s"
        (Location.pr_location (Ident.location_of fid))
        (Ident.pr_ident_name fid) (Ident.pr_ident_name br) (Ident.pr_ident_name l)
  | Const_as_length_use (c, l) ->
      Printf.sprintf "%s: field %s is marked as a constant but used as the length of field %s"
        (Location.pr_location (Ident.location_of c))
        (Ident.pr_ident_name c) (Ident.pr_ident_name l)
  | Variant_as_length_use (v, l) ->
      Printf.sprintf "%s: field %s is marked as a variant but used as the length of field %s"
        (Location.pr_location (Ident.location_of v))
        (Ident.pr_ident_name v) (Ident.pr_ident_name l)

let multiple_length_use fid l1 l2 =
  Multiple_length_use (fid, l1, l2)
let non_local_length_use fid l =
  Non_local_length_use (fid, l)
let multiple_branch_use fid br1 br2 =
  Multiple_branch_use (fid, br1, br2)
let branch_and_length_use fid br l =
  Branch_and_length_use (fid, br, l)
let const_as_length_use c l =
  Const_as_length_use (c, l)
let variant_as_length_use v l =
  Variant_as_length_use (v, l)

let analyze_depinfo fid dep =
  let warnings = ref [] in
  let add_warning w =
    warnings := w :: !warnings
  in
    if dep.field_attribs.field_attrib_value = None then begin
      (* if auto-computation is required, ensure that it is possible in
         the absence of a value specifier *)
      (match dep.length_of with
         | l1 :: l2 :: _ ->
             add_warning (multiple_length_use fid (dpath_tail_ident l1)
                            (dpath_tail_ident l2))
         | [ l ] ->
             if not (is_local_dpath l) then
               add_warning (non_local_length_use fid (dpath_tail_ident l))
         | [] -> ());
      (match dep.length_of, dep.in_length_of with
         | (l1 :: _), (l2 :: _) ->
             add_warning (multiple_length_use fid (dpath_tail_ident l1)
                            (dpath_tail_ident l2))
         | _ -> ());
      (match dep.branch_of with
         | br1 :: br2 :: _ ->
             add_warning (multiple_branch_use fid (dpath_tail_ident br1)
                            (dpath_tail_ident br2))
         | _ -> ());
      (match dep.branch_of, dep.length_of with
         | (br :: _), (l :: _) ->
             add_warning (branch_and_length_use fid (dpath_tail_ident br)
                            (dpath_tail_ident l))
         | _ -> ());
      (match dep.branch_of, dep.in_length_of with
         | (br :: _), (l :: _) ->
             add_warning (branch_and_length_use fid (dpath_tail_ident br)
                            (dpath_tail_ident l))
         | _ -> ());
      (match dep.field_attribs.field_attrib_const, dep.length_of with
         | Some _, (l :: _) ->
             add_warning (const_as_length_use fid (dpath_tail_ident l))
         | _ -> ());
      (match dep.field_attribs.field_attrib_variant, dep.length_of with
         | Some _, (l :: _) ->
             add_warning (variant_as_length_use fid (dpath_tail_ident l))
         | _ -> ());
    end;
    if List.length !warnings > 0 then begin
      if !Config.show_dependency_warnings then
        List.iter (fun w ->
                     Printf.fprintf stderr "%s\n" (warnmsg w)
                  ) !warnings
    end else
      dep.autocompute <- is_scalar dep.field_type

let analyze_formats fmts =
  (* TODO: cyclic dependency analysis *)
  try
    Ident.iter (fun _ st ->
                  let deps = generate_depinfo st in
                    Ident.iter (fun fid dep ->
                                  analyze_depinfo fid dep
                               ) deps
               ) fmts
  with
    | e -> raise e
