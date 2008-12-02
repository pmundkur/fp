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

type variant = (exp * Asttypes.case_name * Asttypes.default) list

type case_exp =
  | Tcase_const of exp
  | Tcase_range of exp * exp

(* constructed types for fields *)

module StringMap: Map.S with type key = string

type branch_value = (path * Asttypes.case_name * struct_type) list * exp

and field_value =
  | Tvalue_default of exp
  | Tvalue_branch of branch_value

and field_attrib =
  | Tattrib_max of exp
  | Tattrib_min of exp
  | Tattrib_const of exp
  | Tattrib_default  of exp
  | Tattrib_value of field_value list
  | Tattrib_variant of variant

and field_type =
  | Ttype_base of base_type
  | Ttype_struct of struct_type
  | Ttype_map of exp * map_type
  | Ttype_array of exp * struct_type
  | Ttype_label

and field_entry =
  | Tfield_name of Ident.t * field_type * field_attrib list
  | Tfield_align of int

and field_info = field_type

(* the struct type embeds the environment in which the field
   identifiers are defined.
*)
and struct_type = (field_entry list) * field_info Ident.env

and map_entry = Ident.t * case_exp * struct_type
and map_type = map_entry StringMap.t

(* information stored in environment *)

type variant_info = Ast.variant

type format_info = struct_type

type type_info = primitive * int

(* generate a field identifier map for a struct *)
val ident_map: struct_type -> field_info Ident.env

(* compute free variables in a struct *)
val free_variables: struct_type -> Ident.t list

(* path utilities *)

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
