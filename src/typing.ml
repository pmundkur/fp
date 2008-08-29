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
exception Non_unique_name of string Location.located_node
exception Non_unique_default of string Location.located_node (* second *) * string Location.located_node (* first *)

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
let raise_non_unique_name nm =
  raise (Non_unique_name nm)
let raise_non_unique_default second first =
  raise (Non_unique_default (second, first))

let functions = [
  ("+", ([Texp_int_type; Texp_int_type], Texp_int_type));
  ("-", ([Texp_int_type; Texp_int_type], Texp_int_type));
  ("*", ([Texp_int_type; Texp_int_type], Texp_int_type));
  ("/", ([Texp_int_type; Texp_int_type], Texp_int_type));

  ("byte_sizeof", ([Texp_base_type], Texp_int_type));
  ("bit_sizeof", ([Texp_base_type], Texp_int_type));
  ("length", ([Texp_vector_type], Texp_int_type));
  ("array_size", ([Texp_array_type], Texp_int_type));

  ("offset", ([Texp_field_name], Texp_int_type));

  ("num_set_bits", ([Texp_field_name], Texp_int_type));

  ("remaining", ([Texp_unit_type], Texp_int_type))
]

let populate_functions env =
  List.fold_left
    (fun e (fid, finfo) ->
       Env.add_function fid finfo e)
    env functions

let init_typing_env () =
  populate_functions (Env.new_env ())

let lookup_function_type env fn =
  match Env.lookup_function_by_name env (Location.node_of fn) with
    | None -> raise_unknown_ident fn
    | Some (_, fti) -> fti

let can_int_coerce i as_type =
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

let can_int32_coerce i as_type =
  match as_type with
    | Tprimitive Tprim_bit -> i = Int32.zero || i = Int32.one
    | Tprimitive Tprim_byte -> i >= Int32.zero && i <= (Int32.of_int 255)
    | Tprimitive Tprim_int16 ->
        i >= Int32.of_int (-32768)
        && i <= Int32.of_int 32767
    | Tprimitive Tprim_int32
    | Tprimitive Tprim_int64 -> true
    | Tvector _ -> false

let can_int64_coerce i as_type =
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

let get_field_info env fn =
    match Env.lookup_field_by_name env (Location.node_of fn) with
      | None -> raise_unknown_ident fn
      | Some fi -> fi

let get_field_type env fn =
  let ((_, t), _) = get_field_info env fn in
    t

let rec follow_case_path cn path m =
  let st =
    try
      StringMap.find (Location.node_of cn) m
    with
      | Not_found ->
          raise_unknown_ident cn
  in
    follow_struct_path st path

and follow_struct_path st path =
  match path with
    | Pfield fn -> Tstruct_type st
    | Ppath (fn, cn, p) ->
        let ft =
          try
            StringMap.find (Location.node_of fn) st
          with
            | Not_found ->
                raise_unknown_ident fn
        in
          get_path_type fn cn p ft

and get_path_type fn cn p ft =
  match ft with
    | Tbase_type _
    | Tstruct_type _
    | Tarray_type _
    | Tlabel -> raise_invalid_path fn
    | Tmap_type m -> follow_case_path cn p m

let lookup_var_type env path =
  match path with
    | Pfield fn -> get_field_type env fn
    | Ppath (fn, cn, p) -> get_path_type fn cn p (get_field_type env fn)

let is_field_type_compatible_with_base_type field_type base_type loc =
  match (field_type, base_type) with
    | (Tbase_type bt, base_type) -> bt = base_type
    | (Tstruct_type _, _)
    | (Tmap_type _, _)
    | (Tarray_type _, _)
    | (Tlabel, _) ->
        raise_field_base_type_mismatch field_type base_type loc

let is_field_type_compatible_with_exp_type field_type exp_type loc =
  match (field_type, exp_type) with
    | (Tbase_type (Tprimitive _), Texp_int_type)
    | (Tbase_type (Tvector _), Texp_vector_type)
    | (Tbase_type _ , Texp_base_type)
    | (Tarray_type _, Texp_array_type) -> true
    | _ ->
        raise_field_exp_type_mismatch field_type exp_type loc

let is_exp_type_equal received expected loc =
  if received = expected then true
  else raise_exp_exp_type_mismatch received expected loc

let rec is_exp_const exp =
  match exp.pexp_desc with
    | Punit -> true
    | Pvar _ -> raise_non_const_exp exp.pexp_loc
    | Pconst_int _
    | Pconst_int32 _
    | Pconst_int64 _ -> true
    | Papply (fname, arglist) ->
        List.fold_left
          (fun r a -> r && is_exp_const a)
          true arglist

(* This is used to typecheck expressions in the context of arguments
   to functions, where the type of the expression needs to match the
   type of the argument expected by the function. *)
let rec type_check_exp_as_exp_type env exp as_exp_type =
  let rec exp_typer exp =
    match exp.pexp_desc with
      | Punit ->
          is_exp_type_equal Texp_unit_type as_exp_type exp.pexp_loc
      | Pvar path ->
          (match as_exp_type with
             | Texp_int_type
             | Texp_vector_type
             | Texp_base_type
             | Texp_array_type
             | Texp_unit_type ->
                 (is_field_type_compatible_with_exp_type
                    (lookup_var_type env path) as_exp_type exp.pexp_loc)
             | Texp_field_name -> true)
      | Pconst_int _
      | Pconst_int32 _
      | Pconst_int64 _ ->
          is_exp_type_equal Texp_int_type as_exp_type exp.pexp_loc
      | Papply (fname, arglist) ->
          let (fat, frt) = lookup_function_type env fname in
          let rcvd, expected = List.length arglist, List.length fat in
            if rcvd <> expected then
              raise_arg_count_mismatch fname rcvd expected
            else
              (List.fold_left2
                 (fun r ae at ->
                    r && type_check_exp_as_exp_type env ae at)
                 true arglist fat)
              && is_exp_type_equal frt as_exp_type exp.pexp_loc
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
      | Punit ->
          (is_field_type_compatible_with_exp_type
             (Tbase_type as_base_type) Texp_unit_type exp.pexp_loc)
      | Pvar path ->
           (is_field_type_compatible_with_base_type
              (lookup_var_type env path) as_base_type exp.pexp_loc)
       | Pconst_int i -> can_int_coerce i as_base_type
       | Pconst_int32 i -> can_int32_coerce i as_base_type
       | Pconst_int64 i -> can_int64_coerce i as_base_type
       | Papply (fname, arglist) ->
           let (fat, frt) = lookup_function_type env fname in
           let rcvd, expected = List.length arglist, List.length fat in
             if rcvd <> expected then
               raise_arg_count_mismatch fname rcvd expected
             else
               (List.fold_left2
                  (fun r ae at ->
                     r && type_check_exp_as_exp_type env ae at)
                  true arglist fat)
               && (is_field_type_compatible_with_exp_type
                     (Tbase_type as_base_type) frt exp.pexp_loc)
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
      ignore (is_exp_const ce);
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

let type_check decls env =
  let typer decl =
    match decl.pdecl_desc with
      | Pvariant (vn, vd) ->
          check_variant_def vd.pvariant_desc;
          Env.add_variant_def (Location.node_of vn) vd env
      | _ (* TODO *) ->
          env
  in
    (* TODO *)
    typer
