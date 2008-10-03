open Asttypes

type decl =
    { pdecl_desc: decl_desc;
      pdecl_loc: Location.t }

and decl_desc =
  | Pdecl_variant of decl_name * variant
  | Pdecl_format of decl_name * format

and variant =
    { pvariant_desc: variant_desc;
      pvariant_loc: Location.t }

and variant_desc = (exp * case_name * default) list

and format =
    { pformat_desc: format_desc;
      pformat_loc: Location.t }

and format_desc = field list

and field =
    { pfield_desc: field_desc;
      pfield_loc: Location.t }

and field_desc =
  | Pfield_name of field_name * field_type
  | Pfield_align of exp

and field_type =
    { pfield_type_desc: field_type_desc;
      pfield_type_loc: Location.t }

and field_type_desc =
  | Ptype_simple of type_exp * field_attrib list
  | Ptype_array of exp * format
  | Ptype_classify of exp * classify_case list
  | Ptype_label

and field_attrib =
    { pfield_attrib_desc: field_attrib_desc;
      pfield_attrib_loc: Location.t }

and field_attrib_desc =
  | Pattrib_max of exp
  | Pattrib_min of exp
  | Pattrib_const of exp
  | Pattrib_default  of exp
  | Pattrib_value of exp
  | Pattrib_variant_ref of decl_name
  | Pattrib_variant_inline of variant

and type_exp =
    { ptype_exp_desc: type_exp_desc;
      ptype_exp_loc: Location.t }

and type_exp_desc =
  | Pbase of type_name
  | Pvector of type_name * exp

and classify_case = case_name * case_exp * format

and case_exp =
    { pcase_exp_desc: case_exp_desc;
      pcase_exp_loc: Location.t }

and case_exp_desc =
  | Pcase_const of exp
  | Pcase_range of exp * exp

and exp =
    { pexp_desc: exp_desc;
      pexp_loc: Location.t }

and exp_desc =
  | Pexp_unit
  | Pexp_var of path
  | Pexp_const_int of int
  | Pexp_const_int32 of Int32.t
  | Pexp_const_int64 of Int64.t
  | Pexp_apply of fun_name * exp list

and path =
  | Pfield of field_name
  | Ppath of field_name * case_name * path

let rec pr_path oc p =
  match p with
    | Pfield fn ->
        Printf.fprintf oc "%a" Location.pr_nloc fn
    | Ppath (fn, cn, p) ->
        Printf.fprintf oc "%a[%a]%a"
          Location.pr_nloc fn
          Location.pr_nloc cn
          pr_path p

let rec pr_exp oc e =
  match e.pexp_desc with
    | Pexp_unit -> Printf.fprintf oc "unit(%a)" Location.pr_loc e.pexp_loc
    | Pexp_var p -> Printf.fprintf oc "%a" pr_path p
    | Pexp_const_int i -> Printf.fprintf oc "%d(%a)" i Location.pr_loc e.pexp_loc
    | Pexp_const_int32 i -> Printf.fprintf oc "%ld(%a)" i Location.pr_loc e.pexp_loc
    | Pexp_const_int64 i -> Printf.fprintf oc "%Ld(%a)" i Location.pr_loc e.pexp_loc
    | Pexp_apply (fn, el) ->
        Printf.fprintf oc "%a ( " Location.pr_nloc fn;
        List.iter (fun e -> pr_exp oc e; Printf.fprintf oc " ") el;
        Printf.fprintf oc " )"

let pr_type_exp oc te =
  match te.ptype_exp_desc with
    | Pbase tn -> Printf.fprintf oc "%a" Location.pr_nloc tn
    | Pvector (tn, e) ->
        Printf.fprintf oc "(%a [ %a ] (%a))"
          Location.pr_nloc tn
          pr_exp e
          Location.pr_loc te.ptype_exp_loc

let pr_variant oc v =
  let pr_case (e, cn, def) =
    Printf.fprintf oc "|%a %s %a "
      pr_exp e
      (if def then "=>" else "->")
      Location.pr_nloc cn in
    begin
      Printf.fprintf oc "{ ";
      List.iter (fun c -> pr_case c) v.pvariant_desc;
      Printf.fprintf oc " }(%a)" Location.pr_loc v.pvariant_loc
    end

let pr_field_attrib oc a =
  match a.pfield_attrib_desc with
    | Pattrib_max e ->
        Printf.fprintf oc "(max(%a) (%a))" pr_exp e Location.pr_loc a.pfield_attrib_loc
    | Pattrib_min e ->
        Printf.fprintf oc "(min(%a) (%a))" pr_exp e Location.pr_loc a.pfield_attrib_loc
    | Pattrib_const e ->
        Printf.fprintf oc "(const(%a) (%a))" pr_exp e Location.pr_loc a.pfield_attrib_loc
    | Pattrib_default e ->
        Printf.fprintf oc "(default(%a) (%a))" pr_exp e Location.pr_loc a.pfield_attrib_loc
    | Pattrib_value e ->
        Printf.fprintf oc "(value(%a) (%a))" pr_exp e Location.pr_loc a.pfield_attrib_loc
    | Pattrib_variant_ref dn ->
        Printf.fprintf oc "(variant %a (%a))" Location.pr_nloc dn Location.pr_loc a.pfield_attrib_loc
    | Pattrib_variant_inline v ->
        Printf.fprintf oc "(variant %a (%a))" pr_variant v Location.pr_loc a.pfield_attrib_loc

let rec pr_field oc f =
  match f.pfield_desc with
    | Pfield_align e ->
        Printf.fprintf oc "align (%a) ; (%a)" pr_exp e Location.pr_loc f.pfield_loc
    | Pfield_name (fn, ft) ->
        Printf.fprintf oc "%a : %a ; (%a)" Location.pr_nloc fn pr_field_type ft Location.pr_loc f.pfield_loc

and pr_field_type oc ft =
  match ft.pfield_type_desc with
    | Ptype_simple (te, al) ->
        Printf.fprintf oc "%a " pr_type_exp te;
        List.iter (fun a -> Printf.fprintf oc "%a " pr_field_attrib a) al
    | Ptype_array (e, fmt) ->
        Printf.fprintf oc "array (%a) { %a }" pr_exp e pr_format fmt
    | Ptype_classify (e, cl) ->
        Printf.fprintf oc "classify (%a) {\n" pr_exp e;
        List.iter (fun (cn, ce, fmt) ->
                     (match ce.pcase_exp_desc with
                       | Pcase_const c ->
                           Printf.fprintf oc "| (%a} : %a -> %a "
                             pr_exp c Location.pr_nloc cn pr_format fmt
                       | Pcase_range (l, r) ->
                           Printf.fprintf oc "| (%a) .. (%a) : %a -> {\n %a \n}"
                             pr_exp l pr_exp r Location.pr_nloc cn pr_format fmt);
                     Printf.fprintf oc "\n")
          cl
    | Ptype_label -> Printf.fprintf oc "label"

and pr_format oc fmt =
  Printf.fprintf oc "{\n";
  List.iter (fun f -> Printf.fprintf oc "%a\n" pr_field f) fmt.pformat_desc;
  Printf.fprintf oc "}(%a)\n" Location.pr_loc fmt.pformat_loc

let pr_decl oc decl =
  match decl.pdecl_desc with
    | Pdecl_variant (dn, v) ->
        Printf.fprintf oc "def variant %a %a\n" Location.pr_nloc dn pr_variant v
    | Pdecl_format (dn, fmt) ->
        Printf.fprintf oc "format %a %a\n" Location.pr_nloc dn pr_format fmt
