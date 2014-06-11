(**************************************************************************)
(*  Copyright 2009-2014       Prashanth Mundkur.                          *)
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

open Types

(* This section of this file checks that the branches specified in
   value attributes are exhaustive and not redundant.  We do this by
   viewing branch specifications as patterns.

   Each branch of the value attribute is viewed as a struct pattern.
   A struct pattern is a list of branch patterns, as is a pattern
   vector.  A pattern matrix is composed of rows of pattern vectors.

   The algorithms implemented below are taken from "Warnings for
   pattern matching", by Luc Maranget, in "Journal of Functional
   Programming", vol 17, issue 3, May 2007.
*)

let list_take n list =
  if n < 0 then raise (Invalid_argument "take");
  let rec take acc n = function
    | [] ->
        if n > 0 then raise (Failure "take") else List.rev acc, []
    | hd :: tl as l ->
        if n = 0 then List.rev acc, l else take (hd :: acc) (n - 1) tl
  in
    take [] n list

let fields = function
  | Tstruct st      -> st.fields
  | Tstruct_named _ -> Ident.empty_env

let classify_fields = function
  | Tstruct st      -> st.classify_fields
  | Tstruct_named _ -> []

let get_case_branch_info branch_info case_name =
  let cn, _, st =
    try
      StringMap.find case_name branch_info.branch_map.map_type_desc
    with
      | Not_found ->
          raise (Failure ("get_case_branch_info: " ^ case_name))
  in
    cn, (classify_fields st)

let make_default binfo =
  { pattern = Pt_any;
    branch_info = binfo }

let make_default_vector binfos =
  List.map make_default binfos

let make_default_from_branch branch_info case_name =
  make_default_vector (snd (get_case_branch_info branch_info case_name))

let specialize_branch_info branch_info case_name =
  snd (get_case_branch_info branch_info case_name)

let rec specialize_vector branch_info case_name = function
  | [] ->
      Some []
  | { pattern = Pt_constructor (cn, (Pt_struct plist)) } :: ptail ->
      if case_name = Location.node_of cn
      then Some (plist @ ptail)
      else None
  | { pattern = Pt_any } :: ptail ->
      Some ((make_default_from_branch branch_info case_name) @ ptail)

and specialize_matrix branch_info case_name matrix =
  List.fold_left
    (fun acc pvec ->
       match (specialize_vector branch_info case_name pvec) with
         | None -> acc
         | Some v -> v :: acc)
    [] (List.rev matrix)

let unspecialize_vector branch_info case_name pvec =
  let cn, cbi = get_case_branch_info branch_info case_name in
  let plist, ptail = list_take (List.length cbi) pvec in
    { pattern = Pt_constructor (cn, (Pt_struct plist));
      branch_info = branch_info } :: ptail

let default_matrix matrix =
  List.fold_left
    (fun acc -> function
       | { pattern = Pt_any } :: ptail -> ptail :: acc
       | _ -> acc)
    [] (List.rev matrix)

module StringSet = Set.Make (struct type t = string let compare = compare end)

let column_signature matrix =
  let do_row set = function
    | { pattern = Pt_constructor (cn, _) } :: ptail ->
        StringSet.add (Location.node_of cn) set
    | _ ->
        set
  in
    List.fold_left do_row StringSet.empty matrix

exception Found of Asttypes.case_name * branch_info list
let get_a_missing_constructor signature branch_info =
  StringSet.iter
    (fun s -> assert (StringMap.mem s branch_info.branch_map.map_type_desc))
    signature;
  try
    StringMap.iter
      (fun s (cn, _, st) ->
         if StringSet.mem s signature
         then ()
         else raise (Found (cn, classify_fields st)))
      branch_info.branch_map.map_type_desc;
    None
  with
    | Found (cn, binfos) -> Some (cn, binfos)

let is_complete_signature signature branch_info =
  match get_a_missing_constructor signature branch_info with
    | None -> true
    | Some _ -> false

(* This is the core usefulness routine, and implements the U_rec
   function of the paper.
*)
let rec is_useful_pattern matrix pattern =
  match matrix with
    | [] ->
        (* This case arises when the first pattern with a certain
           constructor is introduced. *)
        true
    | [] :: ps ->
        (* This case arises when specialized matrices are generated
           for leaf branches, i.e. structs that don't have any
           classify fields.  At this stage, the pattern is completely
           matched by a matrix row, and hence is not useful. *)
        List.iter (fun p -> assert (List.length p = 0)) ps;
        assert (List.length pattern = 0);
        false
    | p :: ps ->
        List.iter (fun p -> assert (List.length p = List.length pattern)) matrix;
        match pattern with
          | { pattern = Pt_constructor (cn, (Pt_struct plist));
              branch_info = bi }
            :: ptail ->
              let sp_matrix =
                specialize_matrix bi (Location.node_of cn) matrix
              in
                is_useful_pattern sp_matrix (plist @ ptail)
          | { pattern = Pt_any; branch_info = bi }
            :: ptail ->
              let signature = column_signature matrix in
                if is_complete_signature signature bi then
                  let sp_input =
                    StringSet.fold
                      (fun s acc ->
                         (specialize_matrix bi s matrix,
                          (make_default_from_branch bi s) @ ptail)
                         :: acc)
                      signature []
                  in
                    List.fold_left
                      (fun acc (m, v) -> acc || is_useful_pattern m v)
                      false sp_input
                else
                  is_useful_pattern (default_matrix matrix) ptail
          | [] ->
              raise (Failure "mismatched pattern/matrix sizes")


(* This is the core exhaustiveness checking routine, and implements
   the I algorithm of the paper.
*)
exception Found of string * branch list
let rec get_unmatched_pattern matrix binfos =
  match matrix with
    | [] ->
        Some (make_default_vector binfos)
    | [] :: ps ->
        List.iter (fun p -> assert (List.length p = 0)) ps;
        assert (List.length binfos = 0);
        None
    | p :: ps ->
        List.iter (fun p' -> assert (List.length p' = List.length p)) ps;
        assert (List.length binfos = List.length p);
        let signature = column_signature matrix in
        let bi, tl_bis = List.hd binfos, List.tl binfos in
        let sp_matrix s = specialize_matrix bi s matrix in
        let sp_bi s = (specialize_branch_info bi s) @ tl_bis in
          if is_complete_signature signature bi then
            begin
              try
                StringSet.iter
                  (fun s ->
                     match get_unmatched_pattern (sp_matrix s) (sp_bi s) with
                       | Some p ->  raise (Found (s, p))
                       | None -> ())
                  signature;
                None
              with
                | Found (s, p) -> Some (unspecialize_vector bi s p)
            end
          else
            match get_unmatched_pattern (default_matrix matrix) tl_bis with
              | None ->
                  None
              | Some p ->
                  let pattern =
                    match get_a_missing_constructor signature bi with
                      | None ->
                          Pt_any
                      | Some (cn, bis) ->
                          Pt_constructor (cn, Pt_struct (make_default_vector bis))
                  in
                    Some ({ pattern = pattern; branch_info = bi } :: p)


(* This is the driver for the pattern usefulness checker.  It takes a
   field_value list as input, and checks whether the specified pattern
   matching has any redundancies and is complete.
*)

exception Redundant_branch_pattern of Location.t
exception Unmatched_branch_pattern of Ident.t * struct_pattern

let raise_redundant_branch_pattern loc =
  raise (Redundant_branch_pattern loc)
let raise_unmatched_branch_pattern id stp =
  raise (Unmatched_branch_pattern (id, stp))

let check_field_value_list fid fvl st =
  let final_matrix =
    List.fold_left
      (fun m fv ->
         (* Note that the pattern computed for the default field_value
            is an empty pattern when the pattern matrix m is empty.
            This will trigger a bunch of asserts if the default is
            followed by any subsequent field values.  But the
            typechecking phase ensures that the default is the last in
            the sequence. *)
         let pattern =
           match fv.field_value_desc with
             | Tvalue_auto
             | Tvalue_default _ ->
                 make_default_vector (classify_fields st)
             | Tvalue_branch { struct_pattern = Pt_struct pattern } ->
                 pattern
         in
           if is_useful_pattern m pattern
           then List.rev (pattern :: (List.rev m))
           else raise_redundant_branch_pattern fv.field_value_loc)
      [] fvl
  in
    match get_unmatched_pattern final_matrix (classify_fields st) with
      | None -> ()
      | Some p -> raise_unmatched_branch_pattern fid (Pt_struct p)

let rec check_struct_patterns st =
  let rec do_field st fid (ft, fas) =
    match ft with
      | Ttype_base _ ->
          do_attribs fid fas st
      | Ttype_struct st ->
          check_struct_patterns st
      | Ttype_map (_, mt) ->
          StringMap.iter
            (fun _ (_, _, st) -> check_struct_patterns st)
            mt.map_type_desc
      | Ttype_array (_, st) ->
          check_struct_patterns st
      | Ttype_format _ ->
          ()
  and do_attribs fid fas st =
    match fas.field_attrib_value with
      | None ->  ()
      | Some (fvl, _) ->  check_field_value_list fid fvl st
  in
    Ident.iter (do_field st) (fields st)


(* This section of the file checks if fields used for classification
   branching have appropriate value attributes.

   - If there are one or more range expressions in the branch
     expressions, and the field has a value attribute, then there must
     be value specifications for the range branch cases, and the
     values must be within the range specified.  It is a warning if
     the field does not have a value attribute.

   - If there are no range expressions, then the field should not have
     any value attribute.

   In the case where the field is a variant, the branch
   expressions should match the variant's values, and any value
   specification for a range should lie within the range.
*)

exception Found of Asttypes.case_name * case_exp
let find_range_case mt =
  try
    StringMap.iter
      (fun _ (cn, ce, _) ->
         match ce.case_exp_desc with
           | Tcase_const _ -> ()
           | Tcase_range _ -> raise (Found (cn, ce)))
      mt.map_type_desc;
    None
  with
    | Found (cn, ce) -> Some (cn, ce)

let branch_match cid cn br =
  br.branch_info.classify_field = cid
  && (match br.pattern with
        | Pt_constructor (bcn, _) ->
            Location.node_of bcn = Location.node_of cn
        | Pt_any ->
            true)

let rec get_matching_values cid cn vl =
  match vl with
    | [] ->
        []
    | v :: vl ->
        (match v.field_value_desc with
           | Tvalue_auto | Tvalue_default _ ->
               v :: (get_matching_values cid cn vl)
           | Tvalue_branch { struct_pattern = Pt_struct bl } ->
               if (List.exists
                     (fun b -> branch_match cid cn b)
                     bl)
               then v :: (get_matching_values cid cn vl)
               else get_matching_values cid cn vl)

exception Field_needs_value_for_range of Ident.t * Asttypes.case_name * case_exp
exception Field_value_mismatch of Ident.t * field_value * Asttypes.case_name * case_exp
exception Field_value_out_of_range of Ident.t * field_value * Asttypes.case_name * case_exp
exception Overspecified_case of Ident.t * Asttypes.case_name * Location.t
exception Unnecessary_field_value of Ident.t * Location.t
exception Unmatched_variant_case of Ident.t * Ident.t * exp
exception Unmatched_classify_case of Ident.t * Ident.t * case_exp
exception Invalid_variant_value of Ident.t * exp

let raise_field_needs_value_for_range bid cn ce =
  raise (Field_needs_value_for_range (bid, cn, ce))
let raise_field_value_mismatch bid v cn ce =
  raise (Field_value_mismatch (bid, v, cn, ce))
let raise_field_value_out_of_range bid v cn ce =
  raise (Field_value_out_of_range (bid, v, cn, ce))
let raise_overspecified_case bid cn loc =
  raise (Overspecified_case (bid, cn, loc))
let raise_unnecessary_field_value bid loc =
  raise (Unnecessary_field_value (bid, loc))
let raise_unmatched_variant_case vid cid e =
  raise (Unmatched_variant_case (vid, cid, e))
let raise_unmatched_classify_case vid cid ce =
  raise (Unmatched_classify_case (vid, cid, ce))
let raise_invalid_variant_value vid e =
  raise (Invalid_variant_value (vid, e))

(* This is the main routine that ensures that a classify case has an
   appropriate value specification for the corresponding branch field.
*)

let check_case (cid, cn, ce) (bid, vl) =
  let mv = get_matching_values cid cn vl in
  let num_mv = List.length mv in
  let mbr =
    List.filter
      (fun v -> match v.field_value_desc with
         | Tvalue_auto | Tvalue_default _ -> false
         | Tvalue_branch _ -> true)
      mv in
  let num_mbr = List.length mbr in
  let check_overspecified_br v =
    match v.field_value_desc with
      | Tvalue_auto | Tvalue_default _ ->
          ()
      | Tvalue_branch { struct_pattern = (Pt_struct bl) } ->
          List.iter
            (fun b ->
               if (b.branch_info.classify_field <> cid)
                 && (b.pattern <> Pt_any)
               then raise_overspecified_case bid cn v.field_value_loc)
            bl in
  let rec checker vl =
    match ce.case_exp_desc, vl with
      | Tcase_const _, [] ->
          ()
      | Tcase_range _, [] ->
          (* There's no error if we merely reached the end of non-empty list. *)
          if num_mv = 0 then
            raise_field_needs_value_for_range bid cn ce
      | Tcase_const c, v :: vl ->
          (match v.field_value_desc with
             | Tvalue_auto ->
                 checker vl
             | Tvalue_default e ->
                 (* Note: the (num_mv = 1) condition assumes that the
                    pattern exhaustiveness has been checked prior to
                    this.  If so, the condition implies that the
                    default is the only match for this branch case. *)
                 if num_mv = 1 && not (exp_value_equal c e)
                 then raise_field_value_mismatch bid v cn ce
                 else checker vl
             | Tvalue_branch bv ->
                 if not (exp_value_equal c bv.value)
                 then raise_field_value_mismatch bid v cn ce
                 else checker vl)
      | Tcase_range (st, fi), v :: vl ->
          (match v.field_value_desc with
             | Tvalue_auto ->
                 if num_mv = 1
                 then raise_field_needs_value_for_range bid cn ce
                 else checker vl
             | Tvalue_default e ->
                 (* See above note on (num_mv = 1). *)
                 if num_mv = 1 && is_const_exp e
                   && not (exp_within_range ~start:st ~finish:fi e)
                 then raise_field_value_out_of_range bid v cn ce
                 else checker vl
             | Tvalue_branch bv ->
                 if is_const_exp bv.value
                   && not (exp_within_range ~start:st ~finish:fi bv.value)
                 then raise_field_value_out_of_range bid v cn ce
                 else checker vl)
  in
    if num_mbr > 1 then
      raise_overspecified_case bid cn (List.nth mbr 2).field_value_loc;
    List.iter check_overspecified_br mbr;
    checker mv

let check_value_list field_env (bid, vl) (cid, mt) =
  StringMap.iter
    (fun _ (cn, ce, st) -> check_case (cid, cn, ce) (bid, vl))
    mt.map_type_desc

let check_variant_cases (bid, v) (cid, mt) vl_opt =
  let cases_contain e =
    StringMap.fold
      (fun _ (_, ce, _) b ->
         b || (match ce.case_exp_desc with
                 | Tcase_const c ->
                     exp_value_equal c e
                 | Tcase_range (start, finish) ->
                     exp_within_range ~start ~finish e))
      mt.map_type_desc false in
  let variant_contains_e e =
    List.fold_left (fun b (ve, _, _) -> b || exp_value_equal e ve) false v.variant_desc in
  let variant_contains_ce ce =
      match ce.case_exp_desc with
        | Tcase_const c ->
            variant_contains_e c
        | Tcase_range (start, finish) ->
            (* Approximate this by checking if both 'start' and
               'finish' are included in the variant. *)
            variant_contains_e start && variant_contains_e finish
  in
    List.iter
      (fun (e, _, _) ->
         if not (cases_contain e) then
           raise_unmatched_variant_case bid cid e)
      v.variant_desc;
    StringMap.iter
      (fun _ (_, ce, _) ->
         if not (variant_contains_ce ce) then
           raise_unmatched_classify_case bid cid ce)
      mt.map_type_desc;
    match vl_opt with
      | None ->
          ()
      | Some vl ->
          List.iter
            (fun fv ->
               match fv.field_value_desc with
                 | Tvalue_auto ->
                     ()
                 | Tvalue_default e
                 | Tvalue_branch { value = e } ->
                     if not (variant_contains_e e) then
                       raise_invalid_variant_value bid e)
            vl

let check_branch_values field_env bi =
  let vl, vr =
    match Ident.assoc_by_id field_env bi.branch_field with
      | None ->
          raise (Failure ("check_branch_values: field "
                          ^ (Ident.name_of bi.branch_field)
                          ^ " not found"))
      | Some (ft, fas) ->
          fas.field_attrib_value, fas.field_attrib_variant in
  let rc_opt = find_range_case bi.branch_map
  in
    match vl, vr, rc_opt with
      | None, None, None ->
          ()
      | None, _, Some (cn, ce) ->
          raise_field_needs_value_for_range bi.branch_field cn ce
      | None, Some (v, _), None ->
          check_variant_cases (bi.branch_field, v) (bi.classify_field, bi.branch_map) None
      | Some (vl, vloc), _, None ->
          raise_unnecessary_field_value bi.branch_field vloc
      | Some (vl, _), None, Some _ ->
          check_value_list field_env (bi.branch_field, vl) (bi.classify_field, bi.branch_map)
      | Some (vl, _), Some (v, _), Some _ ->
          check_variant_cases (bi.branch_field, v) (bi.classify_field, bi.branch_map) (Some vl);
          check_value_list field_env (bi.branch_field, vl) (bi.classify_field, bi.branch_map)

let check_struct_branch_field_values st =
  let rec checker field_env st =
    (* Check the top level branch fields. *)
    List.iter (check_branch_values field_env) (classify_fields st);
    (* Now check nested structs. *)
    Ident.iter
      (fun fid (ft, _) ->
         match ft with
           | Ttype_base _ | Ttype_format _ ->
               ()
           | Ttype_map _ ->
               (* This is done via st.classify_fields above, so we
                  skip it here. *)
               ()
           | Ttype_struct st
           | Ttype_array (_, st) ->
               checker (Ident.extend field_env (fields st)) st)
      (fields st)
  in
    checker (fields st) st


(* This is the main external interface of the checker.  We're given a
   list of formats, on which we call the above two checkers, and print
   any errors that result.
*)

let errmsg e =
  match e with
    | Redundant_branch_pattern loc ->
        Printf.sprintf "%s: redundant branch guard" (Location.pr_location loc)
    | Unmatched_branch_pattern (id, sp) ->
        Printf.sprintf "%s: non-exhaustive value guards for field %s (example: %s)"
          (Location.pr_location (Ident.location_of id))
          (Ident.pr_ident_name id) (pr_struct_pattern sp)
    | Field_needs_value_for_range (bid, cn, ce) ->
        Printf.sprintf
          "%s: field %s needs a value specification due to its use in a range in case %s at %s"
          (Location.pr_location (Ident.location_of bid))
          (Ident.pr_ident_name bid)
          (Location.node_of cn) (Location.pr_line_info ce.case_exp_loc)
    | Field_value_mismatch (bid, fv, cn, ce) ->
        Printf.sprintf
          "%s: the value specified for field %s does not match its use in case %s at %s"
          (Location.pr_location fv.field_value_loc)
          (Ident.pr_ident_name bid)
          (Location.node_of cn) (Location.pr_line_info ce.case_exp_loc)
    | Field_value_out_of_range (bid, fv, cn, ce) ->
        Printf.sprintf
          "%s: the value specified for field %s is out of the range specified for case %s at %s"
          (Location.pr_location fv.field_value_loc)
          (Ident.pr_ident_name bid)
          (Location.node_of cn) (Location.pr_line_info ce.case_exp_loc)
    | Overspecified_case (bid, cn, loc) ->
        Printf.sprintf "%s: field %s is overspecified for case %s"
          (Location.pr_location loc)
          (Ident.pr_ident_name bid) (Location.node_of cn)
    | Unnecessary_field_value (bid, loc) ->
        Printf.sprintf "%s: field %s has an auto-computed value"
          (Location.pr_location loc) (Ident.pr_ident_name bid)
    | Unmatched_variant_case (vid, cid, e) ->
        Printf.sprintf "%s: variant case %s of field %s is not contained in the cases of %s"
          (Location.pr_location e.exp_loc) (pr_exp_desc e.exp_desc)
          (Ident.pr_ident_name vid) (Ident.pr_ident_name cid)
    | Unmatched_classify_case (vid, cid, ce) ->
        Printf.sprintf "%s: classify case %s of field %s is not contained in the variants of %s"
          (Location.pr_location ce.case_exp_loc) (pr_case_exp_desc ce.case_exp_desc)
          (Ident.pr_ident_name cid) (Ident.pr_ident_name vid)
    | Invalid_variant_value (vid, e) ->
        Printf.sprintf "%s: expression does not match any of the variants of %s"
          (Location.pr_location e.exp_loc) (Ident.pr_ident_name vid)
    | e ->
        raise e

let handle_pattern_exception e =
  Printf.fprintf stderr "%s\n" (errmsg e);
  Util.exit_with_code 1

let check_formats fmts =
  try
    Ident.iter (fun _ st ->
                  check_struct_patterns st;
                  check_struct_branch_field_values st;
               ) fmts
  with
    | e -> handle_pattern_exception e
