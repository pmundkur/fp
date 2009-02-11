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
    { exp_desc: exp_desc;
      exp_loc: Location.t }

and exp_desc =
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

let is_const_exp e =
  match e.exp_desc with
    | Texp_unit
    | Texp_const_bit _
    | Texp_const_byte _
    | Texp_const_int16 _
    | Texp_const_uint16 _
    | Texp_const_int _
    | Texp_const_int32 _
    | Texp_const_uint32 _
    | Texp_const_int64 _
        -> true
    | Texp_var _
    | Texp_apply _
        -> false

type base_type =
  | Tbase_primitive of primitive
  | Tbase_vector of primitive * exp

let max_bit_vector_length = 16

type variant =
    { variant_desc: variant_desc;
      variant_loc: Location.t }

and variant_desc =  (exp * Asttypes.case_name * Asttypes.default) list

type case_exp =
    { case_exp_desc: case_exp_desc;
      case_exp_loc: Location.t }

and case_exp_desc =
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
    { classify_field: Ident.t;
      branch_field: Ident.t;
      branch_map: map_type }

and struct_pattern =
  | Pt_struct of branch list

and branch_value =
    { struct_pattern: struct_pattern;
      value: exp }

and field_value =
    { field_value_desc: field_value_desc;
      field_value_loc: Location.t }

and field_value_desc =
  | Tvalue_auto
  | Tvalue_default of exp
  | Tvalue_branch of branch_value

and field_attrib =
    { field_attrib_desc: field_attrib_desc;
      field_attrib_loc: Location.t }

and field_attrib_desc =
  | Tattrib_max of exp
  | Tattrib_min of exp
  | Tattrib_const of exp
  | Tattrib_default of exp
  | Tattrib_value of field_value list
  | Tattrib_variant of variant

and field_type =
  | Ttype_base of base_type
  | Ttype_struct of struct_type
  | Ttype_map of Ident.t * map_type
  | Ttype_array of exp * struct_type
  | Ttype_label
  | Ttype_format of Asttypes.format_name

and field_entry =
    { field_entry_desc: field_entry_desc;
      field_entry_loc: Location.t }

and field_info = field_type * field_attrib list

and field_entry_desc =
  | Tfield_name of Ident.t * field_info
  | Tfield_align of int

and struct_type =
    { entries: field_entry list;
      fields: field_info Ident.env;
      classify_fields: branch_info list;
      struct_type_loc: Location.t }

and map_entry = Asttypes.case_name * case_exp * struct_type

and map_type =
    { map_type_desc: map_type_desc;
      map_type_loc: Location.t }

and map_type_desc =  map_entry StringMap.t

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
          mt.map_type_desc env
    | Ttype_array (_, st) ->
        Ident.extend env (ident_map st)
    | Ttype_label
    | Ttype_format _ ->
        env in
  let env = st.fields
  in
    Ident.fold
      (fun i (ft, _) env -> do_field env ft)
      env env

(* compute free variables in a struct *)
let free_variables st =
  let is_bound id in_scope = Ident.assoc_by_id in_scope id <> None in
  let free_var id in_scope = if is_bound id in_scope then [] else [ id ] in
  let rec free_vars in_scope = function
    | Texp_unit -> []
    | Texp_var (Tvar_ident id) ->
        free_var id in_scope
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
          (fun acc e -> List.rev_append (free_vars in_scope e.exp_desc) acc)
          [] e_list
  and do_field in_scope = function
    | Ttype_base (Tbase_primitive _) ->
        []
    | Ttype_base (Tbase_vector (_, e)) ->
        free_vars in_scope e.exp_desc
    | Ttype_struct st ->
        let ext_scope = Ident.extend in_scope st.fields in
          do_struct st ext_scope []
    | Ttype_map (bid, mt) ->
        let free_vars_ce in_scope = function
          | Tcase_const e ->
              free_vars in_scope e.exp_desc
          | Tcase_range (s, e) ->
              (free_vars in_scope s.exp_desc) @ (free_vars in_scope e.exp_desc)
        in
          StringMap.fold
            (fun _ (_, ce, st) fv ->
               let acc = free_vars_ce in_scope ce.case_exp_desc in
               let ext_scope = Ident.extend in_scope st.fields in
                 List.rev_append (do_struct st ext_scope acc) fv)
            mt.map_type_desc (free_var bid in_scope)
    | Ttype_array (e, st) ->
        let acc = free_vars in_scope e.exp_desc in
        let ext_scope = Ident.extend in_scope st.fields in
          do_struct st ext_scope acc
    | Ttype_label
    | Ttype_format _ ->
        []
  and do_struct st in_scope acc =
    Ident.fold
      (fun i (ft, _) fv -> List.rev_append (do_field in_scope ft) fv)
      st.fields acc
  in
    do_struct st st.fields []

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
  Ident.exists (fun id _ -> Ident.name_of id = fn) st.fields

let lookup_field_in_struct_env fn st =
  Ident.assoc_by_name st.fields fn

let get_field_type fn st =
  match lookup_field_in_struct_env fn st with
    | None ->
        raise Not_found
    | Some (fid, (ft, _)) ->
        fid, ft

(* range and equality checks *)

let within_bit_range i vlen =
  assert (0 < vlen && vlen <= max_bit_vector_length);
  let mask = (0x1 lsl vlen) - 1 in
    (i land mask) = i

let exp_within_range ~start:st ~finish:fi exp =
  match st.exp_desc, fi.exp_desc, exp.exp_desc with
    | Texp_const_bit s, Texp_const_bit f, Texp_const_bit e
    | Texp_const_bit s, Texp_const_bit f, Texp_const_int e

    | Texp_const_byte s, Texp_const_byte f, Texp_const_byte e
    | Texp_const_byte s, Texp_const_byte f, Texp_const_int e

    | Texp_const_int16 s, Texp_const_int16 f, Texp_const_int16 e
    | Texp_const_int16 s, Texp_const_int16 f, Texp_const_int e

    | Texp_const_uint16 s, Texp_const_uint16 f, Texp_const_uint16 e
    | Texp_const_uint16 s, Texp_const_uint16 f, Texp_const_int e

    | Texp_const_int s, Texp_const_int f, Texp_const_int e ->
        compare s e <= 0 && compare e f <= 0

    | Texp_const_int32 s, Texp_const_int32 f, Texp_const_int32 e ->
        Int32.compare s e <= 0 && Int32.compare e f <= 0
    | Texp_const_int32 s, Texp_const_int32 f, Texp_const_int e ->
        if ((Int32.of_int Pervasives.max_int) <> -1l) then
          (* int has a smaller precision than 32 bits *)
          Int32.compare s (Int32.of_int e) <= 0
          && Int32.compare (Int32.of_int e) f <= 0
        else
          compare (Int32.to_int s) e <= 0
          && compare e (Int32.to_int f) <= 0

    | Texp_const_uint32 s, Texp_const_uint32 f, Texp_const_uint32 e
    | Texp_const_int64 s, Texp_const_int64 f, Texp_const_int64 e ->
        Int64.compare s e <= 0 && Int64.compare e f <= 0

    | Texp_const_uint32 s, Texp_const_uint32 f, Texp_const_int e
    | Texp_const_int64 s, Texp_const_int64 f, Texp_const_int e ->
        Int64.compare s (Int64.of_int e) <= 0
        && Int64.compare (Int64.of_int e) f <= 0

    | _ ->
        Printf.fprintf stderr "Invalid exp combo!\n";
        false

let exp_value_equal e1 e2 =
  let with_right_int e1 e2 =
    match e1.exp_desc, e2.exp_desc with
      | Texp_const_bit l, Texp_const_int r
      | Texp_const_byte l, Texp_const_int r
      | Texp_const_int16 l, Texp_const_int r
      | Texp_const_uint16 l, Texp_const_int r ->
          l = r
      | Texp_const_int32 l, Texp_const_int r ->
          if ((Int32.of_int Pervasives.max_int) <> -1l) then
            (* int has a smaller precision than 32 bits *)
            Int32.compare l (Int32.of_int r) = 0
          else
            compare (Int32.to_int l) r = 0
      | Texp_const_uint32 l, Texp_const_int r
      | Texp_const_int64 l, Texp_const_int r ->
          Int64.compare l (Int64.of_int r) = 0
      | _ ->
          false
  in
    e1.exp_desc = e2.exp_desc || with_right_int e1 e2 || with_right_int e2 e1

(* type coercions *)

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
    | Tbase_vector (Tprim_bit, { exp_desc = Texp_const_int vlen }) ->
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
    | Tbase_vector (Tprim_bit, { exp_desc = Texp_const_int vlen }) ->
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
    | Tbase_vector (Tprim_bit, { exp_desc = Texp_const_int vlen }) ->
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
  | Ttype_format f -> Printf.sprintf "format %s" (Location.node_of f)

let pr_struct_pattern sp =
  let rec collect_branch_paths (cur_path, branch_guards) br =
    match br.pattern with
      | Pt_constructor (cn, sp) ->
          let p = br.branch_info.classify_field :: cur_path in
          let bgs = (p, Location.node_of cn) :: branch_guards in
            collect_struct_paths (p, bgs) sp
      | Pt_any ->
          let p = br.branch_info.classify_field :: cur_path in
            (p, "_") :: branch_guards
  and collect_struct_paths (cur_path, branch_guards) (Pt_struct brs) =
    List.fold_left
      (fun bgs br -> collect_branch_paths (cur_path, bgs) br)
      branch_guards brs in
  let pr_bg (path, cn) =
    let p = String.concat "." (List.map Ident.name_of (List.rev path)) in
      Printf.sprintf "%s = %s" p cn in
  let bgs = List.rev (collect_struct_paths ([], []) sp) in
    String.concat ", " (List.map pr_bg bgs)

let pr_exp_desc = function
  | Texp_unit -> "unit"
  | Texp_var p -> Printf.sprintf "var %s" (pr_path p)
  | Texp_const_bit b -> Printf.sprintf "bit %d" b
  | Texp_const_byte b -> Printf.sprintf "byte %d" b
  | Texp_const_int16 i -> Printf.sprintf "int16 %d" i
  | Texp_const_uint16 i -> Printf.sprintf "uint16 %d" i
  | Texp_const_int i -> Printf.sprintf "int %d" i
  | Texp_const_int32 i -> Printf.sprintf "int32 %ld" i
  | Texp_const_uint32 i -> Printf.sprintf "uint32 %Ld" i
  | Texp_const_int64 i -> Printf.sprintf "int64 %Ld" i
  | Texp_apply (id, _) -> Printf.sprintf "%s()" (Ident.pr_ident_name id)

let pr_case_exp_desc = function
  | Tcase_const e ->
      pr_exp_desc e.exp_desc
  | Tcase_range (s, e) ->
      Printf.sprintf "(%s .. %s)"
        (pr_exp_desc s.exp_desc) (pr_exp_desc e.exp_desc)
