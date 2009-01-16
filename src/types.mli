(* types for predefined functions and their expression arguments *)

type exp_type =
  | Texp_type_int
  | Texp_type_vector
  | Texp_type_base
  | Texp_type_array
  | Texp_type_unit
  | Texp_type_field

type function_info = (exp_type list * exp_type) * (Int64.t list -> Int64.t) option

(* basic types for fields and expressions *)

type primitive =
  | Tprim_bit
  | Tprim_byte
  | Tprim_int16
  | Tprim_uint16
  | Tprim_int32
  | Tprim_uint32
  | Tprim_int64

type path =
  | Tvar_ident of Ident.t
  | Tvar_path of Ident.t * path

type exp =
    { exp_desc: exp_desc;
      exp_loc: Location.t }

and exp_desc =
  | Texp_unit
  | Texp_var of path
  | Texp_const_bit of int
  | Texp_const_byte of int
  | Texp_const_int16 of int
  | Texp_const_uint16 of int
  | Texp_const_int of int
  | Texp_const_int32 of Int32.t
  | Texp_const_uint32 of Int64.t
  | Texp_const_int64 of Int64.t
  | Texp_apply of Ident.t * exp list

type base_type =
  | Tbase_primitive of primitive
  | Tbase_vector of primitive * exp

val max_bit_vector_length: int

type variant =
    { variant_desc: variant_desc;
      variant_loc: Location.t }

and variant_desc = (exp * Asttypes.case_name * Asttypes.default) list

type case_exp =
    { case_exp_desc: case_exp_desc;
      case_exp_loc: Location.t }

and case_exp_desc =
  | Tcase_const of exp
  | Tcase_range of exp * exp

(* constructed types for fields *)

module StringMap: Map.S with type key = string

type branch =
    { pattern: pattern;
      branch_info: branch_info }

and pattern =
  | Pt_constructor of Asttypes.case_name * struct_pattern
  | Pt_any

and branch_info =
    { classify_field: Ident.t;
      branch_field: Ident.t;
      branch_map: map_type }

and struct_pattern =
  | Pt_struct of branch list

and branch_value =
    { struct_pattern: struct_pattern;
      value: exp }

and field_value =
    { field_value_desc: field_value_desc;
      field_value_loc: Location.t }

and field_value_desc =
  | Tvalue_auto
  | Tvalue_default of exp
  | Tvalue_branch of branch_value

and field_attrib =
    { field_attrib_desc: field_attrib_desc;
      field_attrib_loc: Location.t }

and field_attrib_desc =
  | Tattrib_max of exp
  | Tattrib_min of exp
  | Tattrib_const of exp
  | Tattrib_default  of exp
  | Tattrib_value of field_value list
  | Tattrib_variant of variant

and field_type =
  | Ttype_base of base_type
  | Ttype_struct of struct_type
  | Ttype_map of Ident.t * map_type
  | Ttype_array of exp * struct_type
  | Ttype_label

and field_entry =
    { field_entry_desc: field_entry_desc;
      field_entry_loc: Location.t }

and field_info = field_type * field_attrib list

and field_entry_desc =
  | Tfield_name of Ident.t * field_info
  | Tfield_align of int


(* the struct type embeds the environment in which the field
   identifiers are defined.
*)
and struct_type =
    { entries: field_entry list;
      fields: field_info Ident.env;
      classify_fields: branch_info list;
      branch_fields: Ident.t list;
      struct_type_loc: Location.t }

and map_entry = Asttypes.case_name * case_exp * struct_type

and map_type =
    { map_type_desc: map_type_desc;
      map_type_loc: Location.t }

and map_type_desc = map_entry StringMap.t

(* information stored in environment *)

type variant_info = Ast.variant

type format_info = struct_type

type type_info = primitive * int

(* generate a field identifier map for a struct *)
val ident_map: struct_type -> field_info Ident.env

(* compute free variables in a struct *)
val free_variables: struct_type -> Ident.t list

(* path utilities *)
val path_head_ident: path -> Ident.t
val path_tail_ident: path -> Ident.t
val path_decompose: path -> string list
val path_compose: path -> path -> path
val path_location_of: path -> Location.t

(* type utilities *)

val is_field_name_in_struct: string -> struct_type -> bool
val lookup_field_in_struct_env: string -> struct_type -> (Ident.t * field_info) option
val get_field_type: string -> struct_type -> (Ident.t * field_type)

(* type coercions *)

val within_bit_range: int -> int -> bool
val can_coerce_int: int -> base_type -> bool
val can_coerce_int32: Int32.t -> base_type -> bool
val can_coerce_int64: Int64.t -> base_type -> bool

(* printing *)

val pr_path: path -> string
val pr_exp_type: exp_type -> string
val pr_primitive: primitive -> string
val pr_base_type: base_type -> string
val pr_field_type: field_type -> string
val pr_struct_pattern: struct_pattern -> string
