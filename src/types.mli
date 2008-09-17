(* types for predefined functions and their expression arguments *)

type exp_type =
  | Texp_int_type
  | Texp_vector_type
  | Texp_base_type
  | Texp_array_type
  | Texp_unit_type
  | Texp_field_name

type function_info = (exp_type list * exp_type) * (int list -> int) option

(* basic types for fields and expressions *)

type primitive =
  | Tprim_bit
  | Tprim_byte
  | Tprim_int16
  | Tprim_int32
  | Tprim_int64

type texp =
  | Texp_unit
  | Texp_var of Ident.t
  | Texp_const_int of int
  | Texp_const_int32 of Int32.t
  | Texp_const_int64 of Int64.t
  | Texp_apply of Ident.t * texp list

type base_type =
  | Tbase_primitive of primitive
  | Tbase_vector of primitive * texp

type variant = (texp * Asttypes.case_name * Asttypes.default) list

(* constructed types for fields *)

module StringMap: Map.S with type key = string

type field_attrib =
  | Tattrib_max of texp
  | Tattrib_min of texp
  | Tattrib_const of texp
  | Tattrib_default  of texp
  | Tattrib_value of texp
  | Tattrib_variant of variant

type field_type =
  | Ttype_base of base_type
  | Ttype_struct of struct_type
  | Ttype_map of map_type
  | Ttype_array of struct_type
  | Ttype_label

and field_entry =
  | Tfield_name of Ident.t * field_type * field_attrib list
  | Tfield_align of int

and struct_type = field_entry list

and map_entry = Ident.t * struct_type
and map_type = map_entry StringMap.t

(* information stored in environment *)

type variant_info = Ast.variant

type format_info = unit

type field_info = field_type

type type_info = primitive * int

(* type utilities *)

val is_field_name_in_struct: string -> struct_type -> bool
val get_field_type: string -> struct_type -> (Ident.t * field_type)

(* type coercions *)

val can_coerce_int: int -> base_type -> bool
val can_coerce_int32: Int32.t -> base_type -> bool
val can_coerce_int64: Int64.t -> base_type -> bool
