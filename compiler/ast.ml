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


let path_decompose path =
  let rec comps_of cur_comps = function
    | Pfield fn ->
        (Location.node_of fn) :: cur_comps
    | Ppath (fn, p) ->
        comps_of ((Location.node_of fn) :: cur_comps) p
  in
    List.rev (comps_of [] path)

let rec path_split path =
  match path with
    | Pfield _ ->
        (* We should always be called with qualified paths. *)
        assert false
    | Ppath (p, (Pfield s)) ->
        Pfield p, s
    | Ppath (f, p) ->
        let pre, suf = path_split p in
          Ppath (f, pre), suf

let rec path_location_of = function
  | Pfield fn -> (Location.location_of fn)
  | Ppath (f, p) -> Location.span (Location.location_of f) (path_location_of p)


let rec pr_path = function
  | Pfield fn ->
      Printf.sprintf "%s" (Location.node_of fn)
  | Ppath (fn, p) ->
      Printf.sprintf "%s.%s" (Location.node_of fn) (pr_path p)

let rec pr_exp ff e =
  match e.pexp_desc with
    | Pexp_unit -> Format.fprintf ff "unit"
    | Pexp_var p -> Format.fprintf ff "%s" (pr_path p)
    | Pexp_const_int i -> Format.fprintf ff "%d" i
    | Pexp_const_int32 i -> Format.fprintf ff "%ld" i
    | Pexp_const_int64 i -> Format.fprintf ff "%Ld" i
    | Pexp_apply (fn, el) ->
        Format.fprintf ff "%s(" (Location.node_of fn);
        Format.pp_open_hvbox ff 0;
        let rec pargs = function
          | [] -> Format.fprintf ff ")"
          | [ e ] -> pr_exp ff e; Format.fprintf ff ")"
          | eh :: et -> pr_exp ff eh; Format.fprintf ff ", "; pargs et
        in
          pargs el;
          Format.pp_close_box ff ()

let pr_type_exp ff te =
  match te.ptype_exp_desc with
    | Pbase tn -> Format.fprintf ff "%s" (Location.node_of tn)
    | Pvector (tn, e) ->
        Format.fprintf ff "%s[" (Location.node_of tn);
        pr_exp ff e;
        Format.fprintf ff "]"

let pr_variant ff v =
  let pcase (e, cn, def) put_break =
    Format.fprintf ff " | ";
    pr_exp ff e;
    Format.fprintf ff " %s %s" (if def then "=>" else "->") (Location.node_of cn);
    if put_break then Format.fprintf ff "@," in
  let rec pcases = function
    | [] -> ()
    | [ c ] -> pcase c false
    | c :: cs -> pcase c true; pcases cs
  in
    Format.fprintf ff "{@[<v 0>";
    pcases v.pvariant_desc;
    Format.fprintf ff "@]@,}"

let pr_branch_guard ff bg =
  let p, cn = bg.pbranch_guard_desc in
    Format.fprintf ff "%s = %s" (pr_path p) (Location.node_of cn)

let pr_value_case ff vc =
  let rec pbranch_guards = function
    | [] -> ()
    | [ bg ] -> pr_branch_guard ff bg
    | bg :: bgs -> pr_branch_guard ff bg; Format.fprintf ff ", "; pbranch_guards bgs
  in
    match vc.pvalue_case_desc with
      | Pvalue_auto ->
          Format.fprintf ff "| _ -> _"
      | Pvalue_default e ->
          pr_exp ff e
      | Pvalue_branch (bgs, e) ->
          Format.fprintf ff "| ";
          pbranch_guards bgs;
          Format.fprintf ff " -> ";
          pr_exp ff e

let pr_field_attrib ff a =
  match a.pfield_attrib_desc with
    | Pattrib_max e ->
        Format.fprintf ff "max(";
        pr_exp ff e;
        Format.fprintf ff ")"
    | Pattrib_min e ->
        Format.fprintf ff "min(";
        pr_exp ff e;
        Format.fprintf ff ")"
    | Pattrib_const e ->
        Format.fprintf ff "const(";
        pr_exp ff e;
        Format.fprintf ff ")"
    | Pattrib_default e ->
        Format.fprintf ff "default(";
        pr_exp ff e;
        Format.fprintf ff ")"
    | Pattrib_variant_ref dn ->
        Format.fprintf ff "variant %s" (Location.node_of dn)
    | Pattrib_variant_inline v ->
        Format.fprintf ff "variant ";
        pr_variant ff v
    | Pattrib_value vcs ->
        Format.fprintf ff "value(";
        if List.length vcs == 1 then begin
          pr_value_case ff (List.hd vcs);
          Format.fprintf ff ")"
        end else begin
          let rec pvalue_cases = function
            | [] -> ()
            | [ vc ] -> pr_value_case ff vc
            | vc :: vcs -> pr_value_case ff vc; Format.fprintf ff "@,"; pvalue_cases vcs
          in
            Format.fprintf ff "@[<v 0>";
            pvalue_cases vcs;
            Format.fprintf ff "@])"
        end

let rec pr_field ff f =
  Format.pp_open_hbox ff ();
  (match f.pfield_desc with
     | Pfield_align e ->
         Format.fprintf ff "align ";
         pr_exp ff e
    | Pfield_name (fn, ft) ->
        Format.fprintf ff "%s: " (Location.node_of fn);
        pr_field_type ff ft);
  Format.pp_close_box ff ()

and pr_case ff (cn, ce, fmt) =
  Format.fprintf ff "@[<v 4>| ";
  (match ce.pcase_exp_desc with
    | Pcase_const c ->
        pr_exp ff c
    | Pcase_range (l, r) ->
        pr_exp ff l;
        Format.fprintf ff " .. ";
        pr_exp ff r);
  Format.fprintf ff " : %s -> {@," (Location.node_of cn);
  pr_format ff fmt;
  Format.fprintf ff "@]@,}"

and pr_field_type ff ft =
  match ft.pfield_type_desc with
    | Ptype_simple (te, al) ->
        pr_type_exp ff te;
        List.iter (fun a -> Format.fprintf ff " "; pr_field_attrib ff a) al
    | Ptype_array (e, fmt) ->
        Format.fprintf ff "@[<v 4>array (";
        pr_exp ff e;
        Format.fprintf ff ") {@,";
        pr_format ff fmt;
        Format.fprintf ff "@]@\n}"
    | Ptype_classify (e, cl) ->
        let rec pcases = function
          | [] -> ()
          | [ cs ] -> pr_case ff cs
          | ch :: ct -> pr_case ff ch; Format.fprintf ff "@,"; pcases ct
        in
          Format.fprintf ff "@[<v 2>classify (";
          pr_exp ff e;
          Format.fprintf ff ") {@,";
          pcases cl;
          Format.fprintf ff "@]@\n}"
    | Ptype_format f -> Format.fprintf ff "format %s" (Location.node_of f)

and pr_format ff fmt =
  let rec pfields = function
    | [] -> ()
    | [ f ] -> pr_field ff f; Format.fprintf ff ";"
    | fh :: ft -> pr_field ff fh; Format.fprintf ff ";@,"; pfields ft
  in
    pfields fmt.pformat_desc

let pr_decl ff decl =
  match decl.pdecl_desc with
    | Pdecl_variant (dn, v) ->
        Format.fprintf ff "def variant %s " (Location.node_of dn);
        pr_variant ff v;
        Format.fprintf ff "@\n@\n"
    | Pdecl_format (dn, fmt) ->
        Format.fprintf ff "@[<v 4>format %s {@," (Location.node_of dn);
        pr_format ff fmt;
        Format.fprintf ff "@]@,}@\n@\n"
