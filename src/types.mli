(* types for predefined functions and their arguments *)

type arg_type =
  | Targ_int_type
  | Targ_vector_type
  | Targ_base_type
  | Targ_array_type
  | Targ_unit_type
  | Targ_field_name

type function_info = arg_type list * arg_type

(* basic types for fields and expressions *)

type prim_type =
  | Tprim_bit
  | Tprim_byte
  | Tprim_int16
  | Tprim_int32
  | Tprim_int64

type base_type =
  | Tprimitive of prim_type
  | Tvector of prim_type * prim_type

(* constructed types for fields *)

module StringMap: Map.S with type key = string

type field_type =
  | Tbase_type of base_type
  | Tstruct_type of struct_type
  | Tmap_type of map_type
  | Tarray_type of struct_type
  | Tlabel

and struct_type = field_type StringMap.t

and map_type = struct_type StringMap.t


type variant_info = unit

type format_info = unit

type field_info = field_type

