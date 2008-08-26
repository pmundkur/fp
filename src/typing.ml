open Types
open Ast

exception UnknownIdentifier of string * Location.t
exception InvalidPath of string * Location.t

let functions = [
  ("+", ([Arg_int_like; Arg_int_like], Arg_int_like));
  ("-", ([Arg_int_like; Arg_int_like], Arg_int_like));
  ("*", ([Arg_int_like; Arg_int_like], Arg_int_like));
  ("/", ([Arg_int_like; Arg_int_like], Arg_int_like));

  ("sizeof", ([Arg_field_name], Arg_int_like));
  ("offset", ([Arg_field_name], Arg_int_like));

  ("num_set_bits", ([Arg_field_name], Arg_int_like));

  ("remaining", ([Arg_unit], Arg_int_like))
]

let populate_functions env =
  List.fold_left
    (fun e (fid, finfo) ->
       Env.add_function fid finfo e)
    env functions

let init_typing_env () =
  populate_functions (Env.new_env ())

let can_int_coerce i as_type =
  match as_type with
    | Primitive Prim_bit ->
        i = 0 || i = 1
    | Primitive Prim_byte ->
        i >= 0 && i <= 255
    | Primitive Prim_int16 ->
        i >= -32768 && i <= 32767
    | Primitive Prim_int32 ->
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
    | Primitive Prim_int64 ->
        true  (* for now ;-) *)
    | _ -> false

let can_int32_coerce i as_type =
  match as_type with
    | Primitive Prim_bit ->
        i = Int32.zero || i = Int32.one
    | Primitive Prim_byte ->
        i >= Int32.zero && i <= (Int32.of_int 255)
    | Primitive Prim_int16 ->
        i >= Int32.of_int (-32768)
        && i <= Int32.of_int 32767
    | Primitive Prim_int32
    | Primitive Prim_int64 ->
        true
    | Types.Vector _ ->
        false

let can_int64_coerce i as_type =
  match as_type with
    | Primitive Prim_bit ->
        i = Int64.zero || i = Int64.one
    | Primitive Prim_byte ->
        i >= Int64.zero && i <= (Int64.of_int 255)
    | Primitive Prim_int16 ->
        i >= Int64.of_int (-32768)
        && i <= Int64.of_int 32767
    | Primitive Prim_int32 ->
        i >= Int64.of_int32 Int32.min_int
        && i <= Int64.of_int32 Int32.max_int
    | Primitive Prim_int64 ->
        true
    | Types.Vector _ ->
        false

let raise_unknown_ident ln =
  let sym = Location.node_of ln in
  let loc = Location.location_of ln in
    raise (UnknownIdentifier (sym, loc))

let raise_invalid_path ln =
  let sym = Location.node_of ln in
  let loc = Location.location_of ln in
    raise (InvalidPath (sym, loc))

let get_field_info env fn =
    match Env.lookup_field_by_name env (Location.node_of fn) with
      | None ->
          raise_unknown_ident fn
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
    | Field fn ->
        Struct_type st
    | Path (fn, cn, p) ->
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
    | Base_type _
    | Struct_type _
    | Types.Label ->
        raise_invalid_path fn
    | Map_type m ->
        follow_case_path cn p m

let lookup_var_type env path =
  match path with
    | Field fn ->
        get_field_type env fn
    | Path (fn, cn, p) ->
        get_path_type fn cn p (get_field_type env fn)

let is_type_compat field_type base_type =
  match (field_type, base_type) with
    | (Base_type bt, base_type) ->
        bt = base_type
    | (Struct_type _, _) ->
        false
    | (Map_type _, _) ->
        false
    | (Types.Label, _) ->
        false

let type_check_exp env exp as_type =
  let rec exp_typer = function
  | Unit -> false
  | Var path -> is_type_compat (lookup_var_type env path) as_type
  | ConstInt i -> can_int_coerce i as_type
  | ConstInt32 i -> can_int32_coerce i as_type
  | ConstInt64 i -> can_int64_coerce i as_type
  | Apply (fname, arglist) -> (* TODO *) false
  in
    exp_typer exp
