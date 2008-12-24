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

let max_bit_vector_length = 16

type variant = (exp * Asttypes.case_name * Asttypes.default) list

type case_exp =
  | Tcase_const of exp
  | Tcase_range of exp * exp

(* constructed types for fields *)

module StringMap = Map.Make (struct type t = string let compare = compare end)

type branch =
    { pattern: pattern;
      branch_info: branch_info }

and pattern =
  | Pt_constructor of Asttypes.case_name * struct_pattern
  | Pt_any

and branch_info =
    { field: Ident.t;
      field_map: map_type }

and struct_pattern =
  | Pt_struct of branch list

and branch_value =
    { struct_pattern: struct_pattern;
      value: exp }

and field_value =
    { field_value_desc: field_value_desc;
      field_value_loc: Location.t }

and field_value_desc =
  | Tvalue_default of exp
  | Tvalue_branch of branch_value

and field_attrib =
  | Tattrib_max of exp
  | Tattrib_min of exp
  | Tattrib_const of exp
  | Tattrib_default of exp
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

and field_info = field_type  (* stored in environment *)

and struct_type =
    { entries: field_entry list;
      env: field_info Ident.env;
      classify_fields: branch_info list }

and map_entry = Asttypes.case_name * case_exp * struct_type
and map_type = map_entry StringMap.t

(* information stored in environment *)

type variant_info = Ast.variant

type format_info = struct_type

type type_info = primitive * int

(* generate a field identifier map for a struct *)
let rec ident_map st =
  let rec do_field env = function
    | Ttype_base _ ->
        env
    | Ttype_struct st ->
        Ident.extend env (ident_map st)
    | Ttype_map (_, mt) ->
        StringMap.fold
          (fun bn (_, _, st) env ->
             Ident.extend env (ident_map st))
          mt env
    | Ttype_array (_, st) ->
        Ident.extend env (ident_map st)
    | Ttype_label ->
        env in
  let env = st.env
  in
    Ident.fold
      (fun i ft env -> do_field env ft)
      env env

(* compute free variables in a struct *)
let free_variables st =
  let bv = st.env in
  let is_bound id = Ident.assoc_by_id bv id <> None in
  let rec free_vars = function
    | Texp_unit -> []
    | Texp_var (Tvar_ident id) ->
        if is_bound id then [] else [ id ]
    | Texp_var (Tvar_path _) ->
        (* We should never encounter paths since we are (presumably)
           not processing expressions in value attributes. *)
        assert false
    | Texp_const_bit _
    | Texp_const_byte _
    | Texp_const_int16 _
    | Texp_const_uint16 _
    | Texp_const_int _
    | Texp_const_int32 _
    | Texp_const_uint32 _
    | Texp_const_int64 _ -> []
    | Texp_apply (_, e_list) ->
        List.fold_left
          (fun acc e -> List.rev_append (free_vars e) acc)
          [] e_list
  and do_field = function
    | Ttype_base (Tbase_primitive _) ->
        []
    | Ttype_base (Tbase_vector (_, e)) ->
        free_vars e
    | Ttype_struct st ->
        do_struct st []
    | Ttype_map (e, mt) ->
        let free_vars_ce = function
          | Tcase_const e -> free_vars e
          | Tcase_range (s, e) -> (free_vars s) @ (free_vars e)
        in
          StringMap.fold
            (fun _ (_, ce, st) fv ->
               List.rev_append (do_struct st (free_vars_ce ce)) fv)
            mt (free_vars e)
    | Ttype_array (e, st) ->
        do_struct st (free_vars e)
    | Ttype_label ->
        []
  and do_struct st acc =
    Ident.fold
      (fun i ft fv -> List.rev_append (do_field ft) fv)
      st.env acc
  in
    do_struct st []

(* path utilities *)

let path_head_ident = function
  | Tvar_ident id
  | Tvar_path (id, _) -> id

let rec path_tail_ident = function
  | Tvar_ident id -> id
  | Tvar_path (_, p) -> path_tail_ident p

let path_decompose path =
  let rec comps_of cur_comps = function
    | Tvar_ident id ->
        (Ident.name_of id) :: cur_comps
    | Tvar_path (id, p) ->
        comps_of ((Ident.name_of id) :: cur_comps) p
  in
    List.rev (comps_of [] path)

let path_compose pre suf =
  let rec ids_of cur_ids = function
    | Tvar_ident id -> id :: cur_ids
    | Tvar_path (id, p) -> ids_of (id :: cur_ids) p in
  let pre_ids = ids_of [] pre in
    List.fold_left (fun p id -> Tvar_path (id, p)) suf pre_ids

let rec path_location_of = function
  | Tvar_ident id -> Ident.location_of id
  | Tvar_path (id, p) -> Location.span (Ident.location_of id) (path_location_of p)

(* type utilities *)

let is_field_name_in_struct fn st =
  Ident.exists (fun id _ -> Ident.name_of id = fn) st.env

let lookup_field_in_struct_env fn st =
  Ident.assoc_by_name st.env fn

let get_field_type fn st =
  match lookup_field_in_struct_env fn st with
    | None ->
        raise Not_found
    | Some (fid, ft) ->
        fid, ft

(* type coercions *)

let within_bit_range i vlen =
  assert (0 < vlen && vlen <= max_bit_vector_length);
  let mask = (0x1 lsl vlen) - 1 in
    (i land mask) = i

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
    | Tbase_vector (Tprim_bit, Texp_const_int vlen) ->
        within_bit_range i vlen
    | Tbase_vector _ -> false

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
    | Tbase_vector (Tprim_bit, Texp_const_int vlen) ->
        within_bit_range (Int32.to_int i) vlen
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
    | Tbase_vector (Tprim_bit, Texp_const_int vlen) ->
        within_bit_range (Int64.to_int i) vlen
    | Tbase_vector _ -> false

(* printing *)

let rec pr_path = function
  | Tvar_ident id -> Printf.sprintf "%s" (Ident.name_of id)
  | Tvar_path (id, p) -> Printf.sprintf "%s.%s" (Ident.name_of id) (pr_path p)

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

