open Types
open Ast

exception UnknownIdentifier of string Location.located_node
exception InvalidPath of string Location.located_node
exception ArgCountMismatch of fun_name * int (* received *) * int (* expected *)

let raise_unknown_ident ln =
    raise (UnknownIdentifier ln)

let raise_invalid_path ln =
    raise (InvalidPath ln)

let raise_arg_count_mismatch fn rcvd expected =
  raise (ArgCountMismatch (fn, rcvd, expected))

let functions = [
  ("+", ([Targ_int_type; Targ_int_type], Targ_int_type));
  ("-", ([Targ_int_type; Targ_int_type], Targ_int_type));
  ("*", ([Targ_int_type; Targ_int_type], Targ_int_type));
  ("/", ([Targ_int_type; Targ_int_type], Targ_int_type));

  ("byte_sizeof", ([Targ_base_type], Targ_int_type));
  ("bit_sizeof", ([Targ_base_type], Targ_int_type));
  ("length", ([Targ_vector_type], Targ_int_type));
  ("array_size", ([Targ_array_type], Targ_int_type));

  ("offset", ([Targ_field_name], Targ_int_type));

  ("num_set_bits", ([Targ_field_name], Targ_int_type));

  ("remaining", ([Targ_unit_type], Targ_int_type))
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

let is_type_compat field_type base_type =
  match (field_type, base_type) with
    | (Tbase_type bt, base_type) -> bt = base_type
    | (Tstruct_type _, _)
    | (Tmap_type _, _)
    | (Tarray_type _, _)
    | (Tlabel, _) -> false

let is_arg_type_compat field_type arg_type =
  match (field_type, arg_type) with
    | (Tbase_type (Tprimitive _), Targ_int_type)
    | (Tbase_type (Tvector _), Targ_vector_type)
    | (Tbase_type _ , Targ_base_type)
    | (Tarray_type _, Targ_array_type) -> true
    | _ -> false

let rec arg_type_check_exp env exp as_arg_type =
  let rec exp_typer = function
    | Punit -> as_arg_type = Targ_unit_type
    | Pvar path ->
        (match as_arg_type with
           | Targ_int_type
           | Targ_vector_type
           | Targ_base_type
           | Targ_array_type ->
               is_arg_type_compat (lookup_var_type env path) as_arg_type
           | Targ_unit_type -> false
           | Targ_field_name -> true)
    | Pconst_int _
    | Pconst_int32 _
    | Pconst_int64 _ -> as_arg_type = Targ_int_type
    | Papply (fname, arglist) ->
        let (fat, frt) = lookup_function_type env fname in
        let rcvd, expected = List.length arglist, List.length fat in
          if rcvd <> expected then
            raise_arg_count_mismatch fname rcvd expected
          else
            (List.fold_left2
              (fun r ae at ->
                 r && arg_type_check_exp env ae at)
              true arglist fat)
            && frt = as_arg_type
  in
    exp_typer exp

let type_check_exp env exp as_type =
  let rec exp_typer = function
    | Punit -> false
    | Pvar path -> is_type_compat (lookup_var_type env path) as_type
    | Pconst_int i -> can_int_coerce i as_type
    | Pconst_int32 i -> can_int32_coerce i as_type
    | Pconst_int64 i -> can_int64_coerce i as_type
    | Papply (fname, arglist) ->
        let (fat, frt) = lookup_function_type env fname in
        let rcvd, expected = List.length arglist, List.length fat in
          if rcvd <> expected then
            raise_arg_count_mismatch fname rcvd expected
          else
            (List.fold_left2
              (fun r ae at ->
                 r && arg_type_check_exp env ae at)
              true arglist fat)
            && is_arg_type_compat (Tbase_type as_type) frt
  in
    exp_typer exp
