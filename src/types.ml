(* types for predefined functions and their expression arguments *)

type exp_type =
  | Texp_int_type
  | Texp_vector_type
  | Texp_base_type
  | Texp_array_type
  | Texp_unit_type
  | Texp_field_name

type function_info = exp_type list * exp_type

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

module StringMap = Map.Make (struct type t = string let compare = compare end)

type field_type =
  | Tbase_type of base_type
  | Tstruct_type of struct_type
  | Tmap_type of map_type
  | Tarray_type of struct_type
  | Tlabel

and struct_type = field_type StringMap.t

and map_type = struct_type StringMap.t


type variant_info = Ast.variant

type format_info = unit

type field_info = field_type


(*
val make_struct: (string * field_type) list -> struct_type
val has_field: struct_type -> bool
val lookup_field_type: struct_type -> string -> field_type
*)
