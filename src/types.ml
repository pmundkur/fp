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

type exp =
  | Texp_unit
  | Texp_var of Ident.t
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

type variant = (exp * Asttypes.case_name * Asttypes.default) list

type case_exp =
  | Tcase_const of exp
  | Tcase_range of exp * exp

(* constructed types for fields *)

module StringMap = Map.Make (struct type t = string let compare = compare end)

type field_attrib =
  | Tattrib_max of exp
  | Tattrib_min of exp
  | Tattrib_const of exp
  | Tattrib_default  of exp
  | Tattrib_value of exp
  | Tattrib_variant of variant

type field_type =
  | Ttype_base of base_type
  | Ttype_struct of struct_type
  | Ttype_map of exp * map_type
  | Ttype_array of exp * struct_type
  | Ttype_label

and field_entry =
  | Tfield_name of Ident.t * field_type * field_attrib list
  | Tfield_align of int

and field_info = field_type  (* stored in environment *)

and struct_type = (field_entry list) * field_info Ident.env

and map_entry = Ident.t * case_exp * struct_type
and map_type = map_entry StringMap.t

(* information stored in environment *)

type variant_info = Ast.variant

type format_info = struct_type

type type_info = primitive * int

(* type utilities *)

let is_field_name_in_struct fn st =
  List.exists
    (function
       | Tfield_name (id, _, _) -> String.compare (Ident.name_of id) fn = 0
       | Tfield_align _ -> false)
    (fst st)

let get_field_type fn st =
  let rec getter = function
    | [] ->
        raise Not_found
    | Tfield_align _ :: tl ->
        getter tl
    | Tfield_name (id, ft, _) :: tl ->
        if String.compare (Ident.name_of id) fn = 0 then
          id, ft
        else
          getter tl
  in
    getter (fst st)

(* type coercions *)

(* TODO: XXX: handle coercions to bit vectors *)

let can_coerce_int i as_type =
  match as_type with
    | Tbase_primitive Tprim_bit -> i = 0 || i = 1
    | Tbase_primitive Tprim_byte -> i >= 0 && i <= 0xff
    | Tbase_primitive Tprim_int16 -> i >= -32768 && i <= 32767
    | Tbase_primitive Tprim_uint16 -> i >= 0 && i <= 0xffff;
    | Tbase_primitive Tprim_int32 ->
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
    | Tbase_primitive Tprim_uint32 ->
        if ((Int32.of_int Pervasives.max_int) <> -1l) then
          (* int has a smaller precision than 32 bits *)
          true
        else
          (i >= 0) && (i <= Int64.to_int 0xffffffffL)
    | Tbase_primitive Tprim_int64 -> true  (* for now ;-) *)
    | _ -> false

let can_coerce_int32 i as_type =
  match as_type with
    | Tbase_primitive Tprim_bit -> i = Int32.zero || i = Int32.one
    | Tbase_primitive Tprim_byte -> i >= Int32.zero && i <= (Int32.of_int 0xff)
    | Tbase_primitive Tprim_int16 ->
        i >= -32768l
        && i <= 32767l
    | Tbase_primitive Tprim_uint16 ->
        i >= 0l && i <= 0xffffl;
    | Tbase_primitive Tprim_int32 ->
        true
    | Tbase_primitive Tprim_uint32 ->
        i >= 0l
    | Tbase_primitive Tprim_int64 -> true
    | Tbase_vector _ -> false

let can_coerce_int64 i as_type =
  match as_type with
    | Tbase_primitive Tprim_bit -> i = Int64.zero || i = Int64.one
    | Tbase_primitive Tprim_byte -> i >= Int64.zero && i <= (Int64.of_int 0xff)
    | Tbase_primitive Tprim_int16 ->
        i >= -32768L
        && i <= 32767L
    | Tbase_primitive Tprim_uint16 ->
        i >= 0L && i <= 0xffffL
    | Tbase_primitive Tprim_int32 ->
        i >= Int64.of_int32 Int32.min_int
        && i <= Int64.of_int32 Int32.max_int
    | Tbase_primitive Tprim_uint32 ->
        i >= 0L && i <= 0xffffffffL
    | Tbase_primitive Tprim_int64 -> true
    | Tbase_vector _ -> false

(* printing *)

let pr_exp_type = function
  | Texp_type_int -> "int"
  | Texp_type_vector -> "vector"
  | Texp_type_base -> "base"
  | Texp_type_array -> "array"
  | Texp_type_unit -> "unit"
  | Texp_type_field -> "field"

let pr_primitive = function
  | Tprim_bit -> "bit"
  | Tprim_byte -> "byte"
  | Tprim_int16 -> "int16"
  | Tprim_uint16 -> "uint16"
  | Tprim_int32 -> "int32"
  | Tprim_uint32 -> "uint32"
  | Tprim_int64 -> "int64"

let pr_base_type = function
  | Tbase_primitive p -> pr_primitive p
  | Tbase_vector (p, _) -> pr_primitive p ^ "[]"

let pr_field_type = function
  | Ttype_base bt -> pr_base_type bt
  | Ttype_struct _ -> "format"
  | Ttype_map _ -> "map"
  | Ttype_array _ -> "array"
  | Ttype_label -> "label"

