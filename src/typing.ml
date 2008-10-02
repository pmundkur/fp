open Asttypes
open Ast
open Types

exception Unknown_identifier of string Location.located_node
exception Invalid_path of field_name
exception Arg_count_mismatch of fun_name * int (* received *) * int (* expected *)
exception Type_mismatch_field_base of field_type (* received *) * base_type (* expected *) * Location.t
exception Type_mismatch_field_exp of field_type * exp_type * Location.t
exception Type_mismatch_exp_exp of exp_type (* received *) * exp_type (* expected *) * Location.t
exception Non_const_expression of Location.t
exception Non_const_integral_expression of Location.t
exception Non_const_foldable_function of fun_name
exception Invalid_const_expression of primitive * Location.t
exception Non_unique_case_name of case_name
exception Non_unique_case_default of case_name (* second *) * case_name (* first *)
exception Bad_alignment of int (* current alignment *) * int (* required alignment *) * Location.t
exception Invalid_align of int * Location.t
exception Duplicate_field of string Location.located_node
exception Unsupported_classify_expr of Location.t
exception Invalid_classify_expr of Location.t
exception Duplicate_attribute of Ident.t * string * Location.t
exception Invalid_attribute of Ident.t * Location.t
exception Conflicting_attributes of Ident.t * string * string
exception Invalid_variant_type of Ident.t * Location.t
exception Invalid_const_type of Ident.t * Location.t
exception Duplicate_classify_case of case_name

let raise_unknown_ident ln =
  raise (Unknown_identifier ln)
let raise_invalid_path ln =
  raise (Invalid_path ln)
let raise_arg_count_mismatch fn rcvd expected =
  raise (Arg_count_mismatch (fn, rcvd, expected))
let raise_field_base_type_mismatch ft bt loc =
  raise (Type_mismatch_field_base (ft, bt, loc))
let raise_field_exp_type_mismatch ft at loc =
  raise (Type_mismatch_field_exp (ft, at, loc))
let raise_exp_exp_type_mismatch received expected  loc =
  raise (Type_mismatch_exp_exp (received, expected, loc))
let raise_non_const_exp loc =
  raise (Non_const_expression loc)
let raise_non_const_integral_exp loc =
  raise (Non_const_integral_expression loc)
let raise_non_const_foldable_function fn =
  raise (Non_const_foldable_function fn)
let raise_invalid_const_expression pt loc =
  raise (Invalid_const_expression (pt, loc))
let raise_non_unique_case_name nm =
  raise (Non_unique_case_name nm)
let raise_non_unique_case_default second first =
  raise (Non_unique_case_default (second, first))
let raise_bad_alignment cur_align expected loc =
  raise (Bad_alignment (cur_align, expected, loc))
let raise_invalid_align align loc =
  raise (Invalid_align (align, loc))
let raise_duplicate_field fn =
  raise (Duplicate_field fn)
let raise_unsupported_classify_expr loc =
  raise (Unsupported_classify_expr loc)
let raise_invalid_classify_expr loc =
  raise (Invalid_classify_expr loc)
let raise_duplicate_attribute fn an loc =
  raise (Duplicate_attribute (fn, an, loc))
let raise_invalid_attribute fn loc =
  raise (Invalid_attribute (fn, loc))
let raise_conflicting_attributes fn a1 a2 =
  raise (Conflicting_attributes (fn, a1, a2))
let raise_invalid_variant_type id loc =
  raise (Invalid_variant_type (id, loc))
let raise_invalid_const_type id loc =
  raise (Invalid_const_type (id, loc))
let raise_duplicate_classify_case cn =
  raise (Duplicate_classify_case cn)

let binary op l =
  assert ((List.length l) = 2);
  op (List.hd l) (List.hd (List.tl l))

let functions = [
  ("+", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary (Int64.add))));
  ("-", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary (Int64.sub))));
  ("*", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary (Int64.mul))));
  ("/", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary (Int64.div))));

  ("byte_sizeof", (([Texp_type_base], Texp_type_int), None));
  ("bit_sizeof", (([Texp_type_base], Texp_type_int), None));
  ("length", (([Texp_type_vector], Texp_type_int), None));
  ("array_size", (([Texp_type_array], Texp_type_int), None));

  ("offset", (([Texp_type_field], Texp_type_int), None));
  ("num_set_bits", (([Texp_type_field], Texp_type_int), None));
  ("remaining", (([Texp_type_unit], Texp_type_int), None))
]

let base_types = [
  ("bit", (Tprim_bit, 1));
  ("byte", (Tprim_byte, 8));
  ("int16", (Tprim_int16, 16));
  ("int32", (Tprim_int32, 32));
  ("int64", (Tprim_int64, 64))
]

let is_bit_typename tn =
  (Location.node_of tn) = "bit"

let populate_functions env =
  List.fold_left
    (fun e (fn, finfo) ->
       let fid = Ident.make_from_string fn Location.dummy_loc in
         Env.add_function fid finfo e)
    env functions

let populate_base_types env =
  List.fold_left
    (fun e (tn, tinfo) ->
       let tid = Ident.make_from_string tn Location.dummy_loc in
         Env.add_type tid tinfo e)
    env base_types

let init_typing_env () =
  populate_functions (populate_base_types (Env.new_env ()))

let lookup_function_info env fn =
  match Env.lookup_function_by_name env (Location.node_of fn) with
    | None -> raise_unknown_ident fn
    | Some (fid, (fti, _)) -> (fid, fti)

let lookup_function_impl env fn =
  match Env.lookup_function_by_name env (Location.node_of fn) with
    | None -> raise_unknown_ident fn
    | Some (_, (_, f)) -> f

let lookup_typename env tn =
  match Env.lookup_type_by_name env (Location.node_of tn) with
    | None -> raise_unknown_ident tn
    | Some tni -> tni

let get_field_info env fn =
  match Env.lookup_field_by_name env (Location.node_of fn) with
    | None -> raise_unknown_ident fn
    | Some fi -> fi

let rec follow_case_path cn path m =
  let st =
    try
      let _, _, st = StringMap.find (Location.node_of cn) m in
        st
    with
      | Not_found ->
          raise_unknown_ident cn
  in
    follow_struct_path st path

and follow_struct_path st path =
  let get_field_in_st fn st =
    try
      get_field_type (Location.node_of fn) st
    with
      | Not_found ->
          raise_unknown_ident fn
  in
    match path with
      | Pfield fn ->
          get_field_in_st fn st
      | Ppath (fn, cn, p) ->
        let ft = snd (get_field_in_st fn st) in
          get_path_type fn cn p ft

and get_path_type fn cn p ft =
  match ft with
    | Ttype_base _
    | Ttype_struct _
    | Ttype_array _
    | Ttype_label -> raise_invalid_path fn
    | Ttype_map (_, m) -> follow_case_path cn p m

let lookup_var env path =
  match path with
    | Pfield fn ->
        get_field_info env fn
    | Ppath (fn, cn, p) ->
        get_path_type fn cn p (snd (get_field_info env fn))

(* The type compatibility check functions either return true or throw an exception. *)

let check_field_type_compatible_with_base_type field_type base_type loc =
  match (field_type, base_type) with
    | (Ttype_base bt, base_type) when bt = base_type ->
        ()
    | (Ttype_base _, _)
    | (Ttype_struct _, _)
    | (Ttype_map _, _)
    | (Ttype_array _, _)
    | (Ttype_label, _) ->
        raise_field_base_type_mismatch field_type base_type loc

let check_field_type_compatible_with_exp_type field_type exp_type loc =
  match (field_type, exp_type) with
    | (Ttype_base (Tbase_primitive _), Texp_type_int)
    | (Ttype_base (Tbase_vector _), Texp_type_vector)
    | (Ttype_base _ , Texp_type_base)
    | (Ttype_array _, Texp_type_array) ->
        ()
    | _ ->
        raise_field_exp_type_mismatch field_type exp_type loc

let check_exp_type_equal received expected loc =
  if received <> expected then
    raise_exp_exp_type_mismatch received expected loc

let rec check_exp_const exp =
  match exp.pexp_desc with
    | Pexp_unit -> true
    | Pexp_var _ -> raise_non_const_exp exp.pexp_loc
    | Pexp_const_int _
    | Pexp_const_int32 _
    | Pexp_const_int64 _ -> true
    | Pexp_apply (fname, arglist) ->
        List.fold_left
          (fun r a -> r && check_exp_const a)
          true arglist

let rec const_fold_as_int64 env exp =
  match exp.pexp_desc with
    | Pexp_unit
    | Pexp_var _ ->
        raise_non_const_integral_exp exp.pexp_loc
    | Pexp_const_int i -> Int64.of_int i
    | Pexp_const_int32 i -> Int64.of_int32 i
    | Pexp_const_int64 i -> i
    | Pexp_apply (fname, arglist) ->
        let iargs = List.map (const_fold_as_int64 env) arglist in
          match lookup_function_impl env fname with
            | None -> raise_non_const_foldable_function fname
            | Some f -> f iargs

let const_fold_as_bit env exp =
  let i = Int64.to_int (const_fold_as_int64 env exp) in
    if i <> 0 && i <> 1 then
      raise_invalid_const_expression Tprim_bit exp.pexp_loc
    else
      i

let const_fold_as_byte env exp =
  let i = Int64.to_int (const_fold_as_int64 env exp) in
    if i < 0 || i > 255 then
      raise_invalid_const_expression Tprim_byte exp.pexp_loc
    else
      i

let const_fold_as_int16 env exp =
  let i = Int64.to_int (const_fold_as_int64 env exp) in
    if i < -32768 || i > 32767  then
      raise_invalid_const_expression Tprim_byte exp.pexp_loc
    else
      i

let const_fold_as_int32 env exp =
  let i64 = const_fold_as_int64 env exp in
    (* TODO: range check *)
    Int32.of_int (Int64.to_int i64)

let const_fold_as_int env exp =
  let i64 = const_fold_as_int64 env exp in
    (* TODO: range check *)
    Int64.to_int i64

let const_fold_as_base_type env exp bt id loc =
  match bt with
    | Tbase_vector _ -> raise_invalid_const_type id loc
    | Tbase_primitive Tprim_bit ->
        Texp_const_bit (const_fold_as_bit env exp)
    | Tbase_primitive Tprim_byte ->
        Texp_const_byte (const_fold_as_byte env exp)
    | Tbase_primitive Tprim_int16 ->
        Texp_const_int16 (const_fold_as_int16 env exp)
    | Tbase_primitive Tprim_int32 ->
        Texp_const_int32 (const_fold_as_int32 env exp)
    | Tbase_primitive Tprim_int64 ->
        Texp_const_int64 (const_fold_as_int64 env exp)


(* This is used to typecheck expressions in two contexts:
   . arguments to functions, where the type of the expression needs to
     match the type of the argument expected by the function
   . length expressions for vector fields
*)
let rec type_check_exp_as_exp_type env exp as_exp_type =
  let rec exp_typer exp =
    match exp.pexp_desc with
      | Pexp_unit ->
          check_exp_type_equal Texp_type_unit as_exp_type exp.pexp_loc;
          Texp_unit
      | Pexp_var path ->
          let vid =
            match as_exp_type with
              | Texp_type_int
              | Texp_type_vector
              | Texp_type_base
              | Texp_type_array
              | Texp_type_unit ->
                  let vid, vt = lookup_var env path in
                    check_field_type_compatible_with_exp_type
                      vt as_exp_type exp.pexp_loc;
                    vid
              | Texp_type_field ->
                  fst (lookup_var env path)
          in
            Texp_var vid
      | Pexp_const_int i ->
          check_exp_type_equal Texp_type_int as_exp_type exp.pexp_loc;
          Texp_const_int i
      | Pexp_const_int32 i ->
          check_exp_type_equal Texp_type_int as_exp_type exp.pexp_loc;
          Texp_const_int32 i
      | Pexp_const_int64 i ->
          check_exp_type_equal Texp_type_int as_exp_type exp.pexp_loc;
          Texp_const_int64 i
      | Pexp_apply (fname, arglist) ->
          let fid, (fat, frt) = lookup_function_info env fname in
          let rcvd, expected = List.length arglist, List.length fat in
            if rcvd <> expected then
              raise_arg_count_mismatch fname rcvd expected
            else
              let targlist =
                List.map2
                  (fun ae at ->
                     type_check_exp_as_exp_type env ae at)
                  arglist fat
              in
                check_exp_type_equal frt as_exp_type exp.pexp_loc;
                Texp_apply (fid, targlist)
  in
    exp_typer exp

(* This is used to typecheck expressions in the following contexts:
   . value expressions,
   . expressions that are values of variant cases,
   . expressions in classify cases
   In both cases, the type of the expression will need to match the
   type of the field, which will be a base_type. *)
let type_check_exp_as_base_type env exp as_base_type =
  let rec exp_typer exp =
    match exp.pexp_desc with
      | Pexp_unit ->
          check_field_type_compatible_with_exp_type
            (Ttype_base as_base_type) Texp_type_unit exp.pexp_loc;
          Texp_unit
      | Pexp_var path ->
          let vid, vt = lookup_var env path in
            check_field_type_compatible_with_base_type
              vt as_base_type exp.pexp_loc;
            Texp_var vid
      | Pexp_const_int i ->
          ignore (Types.can_coerce_int i as_base_type);
          Texp_const_int i
      | Pexp_const_int32 i ->
          ignore (Types.can_coerce_int32 i as_base_type);
          Texp_const_int32 i
      | Pexp_const_int64 i ->
          ignore (Types.can_coerce_int64 i as_base_type);
          Texp_const_int64 i
      | Pexp_apply (fname, arglist) ->
          let fid, (fat, frt) = lookup_function_info env fname in
          let rcvd, expected = List.length arglist, List.length fat in
            if rcvd <> expected then
              raise_arg_count_mismatch fname rcvd expected
            else
              let targlist =
                (List.map2
                   (fun ae at ->
                      type_check_exp_as_exp_type env ae at)
                   arglist fat)
              in
                check_field_type_compatible_with_exp_type
                  (Ttype_base as_base_type) frt exp.pexp_loc;
                Texp_apply (fid, targlist)
  in
    exp_typer exp

module StringSet = Set.Make (struct type t = string let compare = compare end)

let check_variant_def vc_list =
  (* . the exps should be const
     . the case_names should be distinct
     . there should only be one default
  *)
  let names = ref StringSet.empty in
  let default = ref None in
  let check_case (ce, cn, def) =
    let nm = Location.node_of cn in
      ignore (check_exp_const ce);
      if StringSet.mem nm !names then begin
        raise_non_unique_case_name cn
      end else begin
        names := StringSet.add nm !names;
      end;
      match !default with
        | None -> default := Some cn
        | Some df -> raise_non_unique_case_default cn df
  in
    List.iter check_case vc_list

let is_byte_aligned a =
  a mod 8 = 0

(* This implements the base cases of the kinding relation of
   the specification:
        E, a |- tau : K, a'
   Alignment is computed in units of bits, modulo 8.

   It returns the field_type representation of the base type
   expression.

*)

let kinding env cur_align te =
  match te.ptype_exp_desc with
    | Pbase tn ->
        let _, (pt, ptsize) = lookup_typename env tn in
        let next_align =
          if is_bit_typename tn then
            cur_align + 1
          else if not (is_byte_aligned cur_align) then
            raise_bad_alignment cur_align 8 te.ptype_exp_loc
          else
            cur_align + ptsize
        in
          (Tbase_primitive pt), next_align
    | Pvector (tn, e) ->
        let e' = type_check_exp_as_exp_type env e Texp_type_int in
        let _, (pt, _) = lookup_typename env tn in
        let next_align =
          if is_bit_typename tn then
            cur_align + const_fold_as_int env e
          else if not (is_byte_aligned cur_align) then
            raise_bad_alignment cur_align 8 te.ptype_exp_loc
          else
            0
        in
          Tbase_vector (pt, e'), next_align

type field_check_info =
  | Align of int
  | Field of Ident.t * Ast.field_attrib list

let is_field_name_used fn fl =
  List.exists
    (function
       | Align _ -> false
       | Field (id, _) -> Ident.name_of id = Location.node_of fn)
    fl


let type_check_variant_attrib env f bt v =
  let pt =
    match bt with
      | Tbase_vector _ ->
          raise_invalid_variant_type f v.pvariant_loc
      | Tbase_primitive p -> p in
  let cfold vc =
    match pt with
      | Tprim_bit -> Texp_const_bit (const_fold_as_bit env vc)
      | Tprim_byte -> Texp_const_byte (const_fold_as_byte env vc)
      | Tprim_int16 -> Texp_const_int16 (const_fold_as_int16 env vc)
      | Tprim_int32 -> Texp_const_int32 (const_fold_as_int32 env vc)
      | Tprim_int64 -> Texp_const_int64 (const_fold_as_int64 env vc)
  in
    List.map
      (fun (e, cn, d) -> (cfold e), cn, d)
      v.pvariant_desc


(* This is an extension of the typing of value expressions to the
   typing of field attributes, which were left out of the
   specification to keep it simple.  The caller supplies the adjusted
   environment to enforce the layering principle.

   The implemented rule is a generalization of:

        E |- x : value_exp
*)

let type_attribs env f ft fal =
  let max_present = ref false in
  let min_present = ref false in
  let const_present = ref false in
  let default_present = ref false in
  let value_present = ref false in
  let variant_present = ref false in
  let check_and_mark_present attrib_name flag loc =
    if !flag then
      raise_duplicate_attribute f attrib_name loc
    else
      flag := true in
  let check_flags () =
    (*
      - (max|min) conflicts with const+variant
      - const conflicts with default+value+variant
      - default conflicts with variant
      - value conflicts with variant
    *)
    if !max_present || !min_present then
      if !const_present then
        raise_conflicting_attributes f "range" "const"
      else if !variant_present then
        raise_conflicting_attributes f "range" "variant"
      else
        ()
    else if !const_present then
      if !default_present then
        raise_conflicting_attributes f "const" "default"
      else if !value_present then
        raise_conflicting_attributes f "const" "value"
      else if !variant_present then
        raise_conflicting_attributes f "const" "variant"
      else
        ()
    else if !default_present && !variant_present then
      raise_conflicting_attributes f "default" "variant"
    else if !value_present && !variant_present then
      raise_conflicting_attributes f "value" "variant"
    else
      () in
  let get_variant_def env vn =
    match Env.lookup_variant_by_name env (Location.node_of vn) with
      | None -> raise_unknown_ident vn
      | Some vd -> vd in
  let tal =
    List.map
      (fun fa ->
         match fa.pfield_attrib_desc, ft with
           | Pattrib_max e, Ttype_base bt ->
               check_and_mark_present "max" max_present fa.pfield_attrib_loc;
               ignore (type_check_exp_as_base_type env e bt);
               Tattrib_max (const_fold_as_base_type env e bt f fa.pfield_attrib_loc)
           | Pattrib_min e, Ttype_base bt ->
               check_and_mark_present "min" min_present fa.pfield_attrib_loc;
               ignore (type_check_exp_as_base_type env e bt);
               Tattrib_min (const_fold_as_base_type env e bt f fa.pfield_attrib_loc)
           | Pattrib_const e, Ttype_base bt ->
               check_and_mark_present "const" const_present fa.pfield_attrib_loc;
               ignore (type_check_exp_as_base_type env e bt);
               Tattrib_const (const_fold_as_base_type env e bt f fa.pfield_attrib_loc)
           | Pattrib_default e, Ttype_base bt ->
               check_and_mark_present "default" default_present fa.pfield_attrib_loc;
               Tattrib_default (type_check_exp_as_base_type env e bt)
           | Pattrib_value e, Ttype_base bt ->
               check_and_mark_present "value" value_present fa.pfield_attrib_loc;
               Tattrib_value (type_check_exp_as_base_type env e bt)
           | Pattrib_variant_ref vn, Ttype_base bt ->
               check_and_mark_present "variant" variant_present fa.pfield_attrib_loc;
               let _, vdef = get_variant_def env vn in
                 Tattrib_variant (type_check_variant_attrib env f bt vdef)
           | Pattrib_variant_inline vdef, Ttype_base bt ->
               check_and_mark_present "variant" variant_present fa.pfield_attrib_loc;
               check_variant_def vdef.pvariant_desc;
               Tattrib_variant (type_check_variant_attrib env f bt vdef)
           | _ ->
               raise_invalid_attribute f fa.pfield_attrib_loc;
      )
      fal
  in
    check_flags ();
    tal

(* This implements the field-checking relation:
        E, a, S |- F, E', a', S'

   However, instead of using an explicit name set S, we instead thread
   a list containing information on the fields checked thus far.
*)

let rec type_field (env, cur_align, fl) f =
  match f.pfield_desc with
    | Pfield_align a ->
        let c = const_fold_as_int env a in
          if not (is_byte_aligned c) then
            raise_invalid_align c a.pexp_loc
          else
            env, 0, (Align c) :: fl
    | Pfield_name (fn, ft) ->
        if is_field_name_used fn fl then
          raise_duplicate_field fn;
        let fi = Ident.make_from_node fn in
          match ft.pfield_type_desc with
            | Ptype_simple (te, fal) ->
                let bt, next_align = kinding env cur_align te in
                let e = Env.add_field fi (Ttype_base bt) env in
                  e, next_align, (Field (fi, fal)) :: fl
            | Ptype_label ->
                if not (is_byte_aligned cur_align) then
                  raise_bad_alignment cur_align 8 ft.pfield_type_loc;
                let e = Env.add_field fi Ttype_label env in
                  e, cur_align, (Field (fi, [])) :: fl
            | Ptype_array (len, fmt) ->
                if not (is_byte_aligned cur_align) then
                  raise_bad_alignment cur_align 8 ft.pfield_type_loc
                else begin
                  let tlen = type_check_exp_as_exp_type env len Texp_type_int in
                  let st = type_format env fmt in
                  let e = Env.add_field fi (Ttype_array (tlen, st)) env in
                    e, 0, (Field (fi, [])) :: fl
                end
            | Ptype_classify (e, cl) ->
                if not (is_byte_aligned cur_align) then
                  raise_bad_alignment cur_align 8 ft.pfield_type_loc;
                (* Restrict classification expressions to variables to
                   simplify typing, for now. *)
                let eid, et = match e.pexp_desc with
                  | Pexp_var v ->
                      lookup_var env v
                  | _ ->
                      raise_unsupported_classify_expr e.pexp_loc in
                let bt = match et with
                  | Ttype_base bt -> bt
                  | _ -> raise_invalid_classify_expr e.pexp_loc in
                let tcl =
                  List.fold_left
                    (fun a (cn, ce, fmt) ->
                       let cid = Ident.make_from_node cn in
                       let cnm = Location.node_of cn in
                         if List.mem_assoc cnm a then
                           raise_duplicate_classify_case cn;
                         let te = match ce.pcase_exp_desc with
                           | Pcase_const c ->
                               Tcase_const (const_fold_as_base_type env c bt cid c.pexp_loc)
                           | Pcase_range (l, r) ->
                               let tl = const_fold_as_base_type env l bt cid l.pexp_loc in
                               let tr = const_fold_as_base_type env r bt cid r.pexp_loc in
                                 Tcase_range (tl, tr) in
                         let tfmt = type_format env fmt in
                           (cnm, (cid, te, tfmt)) :: a)
                    [] cl in
                let m = List.fold_left
                  (fun m (nm, (cid, te, tfmt)) ->
                     StringMap.add nm (cid, te, tfmt) m)
                  StringMap.empty tcl in
                let e = Env.add_field fi (Ttype_map (Texp_var eid, m)) env in
                  e, 0, (Field (fi, [])) :: fl

(* This function gets used in the typing of fields that have
   struct-oriented types, i.e. arrays and classifications.

   The layering principle is enforced here, via the adjustment of the
   environment in which the field attributes, especially the value
   expression attribute (i.e. the "value expression" of the
   specification) are typed in.
*)

and type_format env fmt =
  let lookup_type fid env =
    match Env.lookup_field_by_id env fid with
      | None -> assert false
      | Some ft -> ft in
  let type_fields () =
    List.fold_left type_field (env, 0, []) fmt.pformat_desc in
  let get_value_env ext_env fl =
    List.fold_left
      (fun e f ->
         match f with
           | Align _ ->
               e
           | Field (id, _) ->
               Env.add_field id (lookup_type id ext_env) e)
      (init_typing_env ())
      (List.rev fl) in
  let get_field_entries venv fl =
    List.fold_left
      (fun accu f ->
         match f with
           | Align i ->
               (Tfield_align i) :: accu
           | Field (id, al) ->
               let ft = lookup_type id venv in
               let tal = type_attribs venv id ft al in
                 (Tfield_name (id, ft, tal)) :: accu)
      [] fl in
  let ext_env, align, fl = type_fields () in
  let venv = get_value_env ext_env fl in
    (get_field_entries venv fl), (Env.extract_field_env venv)

(* type-checker top-level *)

let type_check env decls =
  let typer e d =
    match d.pdecl_desc with
      | Pdecl_variant (vn, vd) ->
          check_variant_def vd.pvariant_desc;
          Env.add_variant_def (Ident.make_from_node vn) vd e
      | Pdecl_format _ ->
          (* TODO *)
          e
  in
    List.fold_left
      (fun e d -> typer e d) env decls
