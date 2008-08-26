(* types for predefined functions and their arguments *)

type arg_type =
  | Arg_bit_like
  | Arg_int_like
  | Arg_vec_like
  | Arg_array_like
  | Arg_field_name
  | Arg_unit

type function_info = arg_type list * arg_type

(* basic types for fields and expressions *)

type prim_type =
  | Prim_bit
  | Prim_byte
  | Prim_int16
  | Prim_int32
  | Prim_int64

type base_type =
  | Primitive of prim_type
  | Vector of prim_type * prim_type

(* constructed types for fields *)

module StringMap: Map.S with type key = string

type field_type =
  | Base_type of base_type
  | Struct_type of struct_type
  | Map_type of map_type
  | Array_type of struct_type
  | Label

and struct_type = field_type StringMap.t

and map_type = struct_type StringMap.t


type variant_info = unit

type format_info = unit

type field_info = field_type

