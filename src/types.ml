type prim_type =
  | Prim_bit
  | Prim_byte
  | Prim_int16
  | Prim_int32
  | Prim_int64

type base_type =
  | Primitive of prim_type
  | Vector of prim_type * prim_type

type struct_type

type map_type

type field_type =
  | Base_type of base_type
  | Struct_type of struct_type
  | Map_type of map_type
  | Label

type arg_type =
  | Arg_bit_like
  | Arg_int_like
  | Arg_vec_like
  | Arg_field_name
  | Arg_unit

type function_info = arg_type list * arg_type

type variant_info = unit

type format_info = unit

type field_info = field_type


(*
val make_struct: (string * field_type) list -> struct_type
val has_field: struct_type -> bool
val lookup_field_type: struct_type -> string -> field_type
*)
