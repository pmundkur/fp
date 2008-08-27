open Asttypes

type decl =
    { pdecl_desc: decl_desc;
      pdecl_loc: Location.t }

and decl_desc =
  | Pvariant of decl_name * variant
  | Pformat of decl_name * format

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
  | Pnamed_field of field_name * field_type
  | Palign of exp

and field_type =
    { pfield_type_desc: field_type_desc;
      pfield_type_loc: Location.t }

and field_type_desc =
  | Psimple of type_exp * field_attrib list
  | Parray of exp * format
  | Pclassify of exp * classify_case list
  | Plabel

and field_attrib =
    { pfield_attrib_desc: field_attrib_desc;
      pfield_attrib_loc: Location.t }

and field_attrib_desc =
  | Pmax of exp
  | Pmin of exp
  | Pconst of exp
  | Pdefault  of exp
  | Pvalue of exp
  | Pvariant_ref of decl_name
  | Pvariant_inline of variant

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
  | Punit
  | Pvar of path
  | Pconst_int of int
  | Pconst_int32 of Int32.t
  | Pconst_int64 of Int64.t
  | Papply of fun_name * exp list

and path =
  | Pfield of field_name
  | Ppath of field_name * case_name * path
