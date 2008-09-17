open Asttypes
open Ast
open Types

exception Unknown_identifier of string Location.located_node
exception Invalid_path of string Location.located_node
exception Arg_count_mismatch of fun_name * int (* received *) * int (* expected *)
exception Type_mismatch_field_base of field_type (* received *) * base_type (* expected *) * Location.t
exception Type_mismatch_field_exp of field_type * exp_type * Location.t
exception Type_mismatch_exp_exp of exp_type (* received *) * exp_type (* expected *) * Location.t
exception Non_const_expression of Location.t
exception Non_const_integral_expression of Location.t
exception Non_const_foldable_function of string Location.located_node
exception Non_unique_name of string Location.located_node
exception Non_unique_default of string Location.located_node (* second *) * string Location.located_node (* first *)
exception Bad_alignment of int (* current alignment *) * int (* required alignment *) * Location.t
exception Invalid_align of int * Location.t
exception Duplicate_field of string Location.located_node

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
let raise_non_unique_name nm =
  raise (Non_unique_name nm)
let raise_non_unique_default second first =
  raise (Non_unique_default (second, first))
let raise_bad_alignment cur_align expected loc =
  raise (Bad_alignment (cur_align, expected, loc))
let raise_invalid_align align loc =
  raise (Invalid_align (align, loc))
let raise_duplicate_field fn =
  raise (Duplicate_field fn)


let binary op l =
  assert ((List.length l) = 2);
  op (List.hd l) (List.hd (List.tl l))

let functions = [
  ("+", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary (+))));
  ("-", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary (-))));
  ("*", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary ( * ))));
  ("/", (([Texp_type_int; Texp_type_int], Texp_type_int),
         Some (binary (/))));

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
         Env.add_type tid tinfo)
    env base_types

let init_typing_env () =
  populate_functions (Env.new_env ())

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
      snd (StringMap.find (Location.node_of cn) m)
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
    | Ttype_map m -> follow_case_path cn p m

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

let rec const_fold_as_int env exp =
  match exp.pexp_desc with
    | Pexp_unit
    | Pexp_var _ ->
        raise_non_const_integral_exp exp.pexp_loc
    | Pexp_const_int i -> i
    | Pexp_const_int32 i -> Int32.to_int i (* TODO: range check *)
    | Pexp_const_int64 i -> Int64.to_int i (* TODO: range check *)
    | Pexp_apply (fname, arglist) ->
        let iargs = List.map (const_fold_as_int env) arglist in
          match lookup_function_impl env fname with
            | None -> raise_non_const_foldable_function fname
            | Some f -> f iargs

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

(* This is used to typecheck expressions in two contexts:
   . value expressions,
   . expressions that are values of variant cases.
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
        raise_non_unique_name cn
      end else begin
        names := StringSet.add nm !names;
      end;
      match !default with
        | None -> default := Some cn
        | Some df -> raise_non_unique_default cn df
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

(* This implements the field-checking relation:
        E, a, S |- F, E', a', S'

   However, instead of using an explicit name set S, we instead thread
   a list containing information on the fields checked thus far.
*)

type field_check_info =
  | Align of int
  | Field of Ident.t * Ast.field_attrib list

let is_field_name_used fn fl =
  List.exists
    (function
       | Align _ -> false
       | Field (id, _) -> Ident.name_of id = Location.node_of fn)
    fl

let rec field_check (env, cur_align, fl) f =
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
        match ft.pfield_type_desc with
          | Ptype_simple (te, fal) ->
              let bt, next_align = kinding env cur_align te in
              let fi = Ident.make_from_node fn in
              let e = Env.add_field fi (Ttype_base bt) env in
                e, next_align, (Field (fi, fal)) :: fl
          | Ptype_label ->
              if not (is_byte_aligned cur_align) then
                raise_bad_alignment cur_align 8 ft.pfield_type_loc
              else begin
                let fi = Ident.make_from_node fn in
                let e = Env.add_field fi Ttype_label env in
                  e, cur_align, (Field (fi, [])) :: fl
              end
          | Ptype_array (len, fmt) ->
              if not (is_byte_aligned cur_align) then
                raise_bad_alignment cur_align 8 ft.pfield_type_loc
              else begin
                let tlen = type_check_exp_as_exp_type env len Texp_type_int in
                let st = format_check env fmt in
                let fi = Ident.make_from_node fn in
                let e = Env.add_field fi (Ttype_array (tlen, st)) env in
                  e, 0, (Field (fi, [])) :: fl
              end
          | _ -> (* TODO *)
              env, cur_align, fl

and format_check env fmt =
  []

(* type-checker top-level *)

let type_check decls env =
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
