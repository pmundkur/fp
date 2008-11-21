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
  | Pattrib_variant_ref of decl_name
  | Pattrib_variant_inline of variant
  | Pattrib_value of value_case list

and value_case =
    { pvalue_case_desc: value_case_desc;
      pvalue_case_loc: Location.t }

and value_case_desc =
  | Pvalue_default of exp
  | Pvalue_branch of branch_guard list * exp

and branch_guard =
    { pbranch_guard_desc: branch_guard_desc;
      pbranch_guard_loc: Location.t }

and branch_guard_desc = path * case_name

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
  | Ppath of field_name * path

val path_split: path -> (path (* prefix *) * field_name (* suffix *))
val path_decompose: path -> string list
val path_location_of: path -> Location.t

val pr_path: path -> string
val pr_decl: Format.formatter -> decl -> unit

