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

module StringMap = Map.Make (struct type t = string let compare = compare end)

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

(* type kinding *)

type kind =
  | Kprim of prim_type
  | Kvector of prim_type * texp
  | Kstruct
  | Karray
  | Kmap

(* type coercions *)

let can_coerce_int i as_type =
  match as_type with
    | Tprimitive Tprim_bit -> i = 0 || i = 1
    | Tprimitive Tprim_byte -> i >= 0 && i <= 255
    | Tprimitive Tprim_int16 -> i >= -32768 && i <= 32767
    | Tprimitive Tprim_int32 ->
        (* int could have a precision of either 31 or 63 bits.  We
           need to perform the range comparison in the type with
           higher precision. *)
        if ((Int32.of_int Pervasives.max_int) <> -1l) then
          (* int has a smaller precision than 32 bits *)
          true
        else
          (* perform the range comparison in the int type, since it
             has higher precision. *)
          (i >= Int32.to_int Int32.min_int
           && i <= Int32.to_int Int32.max_int)
    | Tprimitive Tprim_int64 -> true  (* for now ;-) *)
    | _ -> false

let can_coerce_int32 i as_type =
  match as_type with
    | Tprimitive Tprim_bit -> i = Int32.zero || i = Int32.one
    | Tprimitive Tprim_byte -> i >= Int32.zero && i <= (Int32.of_int 255)
    | Tprimitive Tprim_int16 ->
        i >= Int32.of_int (-32768)
        && i <= Int32.of_int 32767
    | Tprimitive Tprim_int32
    | Tprimitive Tprim_int64 -> true
    | Tvector _ -> false

let can_coerce_int64 i as_type =
  match as_type with
    | Tprimitive Tprim_bit -> i = Int64.zero || i = Int64.one
    | Tprimitive Tprim_byte -> i >= Int64.zero && i <= (Int64.of_int 255)
    | Tprimitive Tprim_int16 ->
        i >= Int64.of_int (-32768)
        && i <= Int64.of_int 32767
    | Tprimitive Tprim_int32 ->
        i >= Int64.of_int32 Int32.min_int
        && i <= Int64.of_int32 Int32.max_int
    | Tprimitive Tprim_int64 -> true
    | Tvector _ -> false
