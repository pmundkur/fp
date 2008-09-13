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

type prim_type =
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
  | Tprimitive of prim_type
  | Tvector of prim_type * texp

(* constructed types for fields *)

module StringMap: Map.S with type key = string

type field_type =
  | Tbase_type of base_type
  | Tstruct_type of struct_type
  | Tmap_type of map_type
  | Tarray_type of struct_type
  | Tlabel

and struct_entry = Ident.t * field_type
and struct_type = struct_entry StringMap.t

and map_entry = Ident.t * struct_type
and map_type = map_entry StringMap.t

(* information stored in environment *)

type variant_info = Ast.variant

type format_info = unit

type field_info = field_type

type type_info = prim_type * int

(* type coercions *)

val can_coerce_int: int -> base_type -> bool
val can_coerce_int32: Int32.t -> base_type -> bool
val can_coerce_int64: Int64.t -> base_type -> bool
