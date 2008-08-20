open Types
open Ast

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

let check_int_coercion i as_type =
  match as_type with
    | Primitive Prim_bit ->
        i = 0 || i = 1
    | Primitive Prim_byte ->
        i >= 0 && i <= 255
    | Primitive Prim_int16 ->
        i >= -32768 && i <= 32767
    | _ -> (* TODO *) false

let check_int32_coercion i as_type =
  match as_type with
    | Primitive Prim_bit ->
        i = Int32.zero || i = Int32.one
    | Primitive Prim_byte ->
        i >= Int32.zero && i <= (Int32.of_int 255)
    | _ -> (* TODO *) false

let check_int64_coercion i as_type =
  match as_type with
    | Primitive Prim_bit ->
        i = Int64.zero || i = Int64.one
    | Primitive Prim_byte ->
        i >= Int64.zero && i <= (Int64.of_int 255)
    | _ -> (* TODO *) false


let type_check_exp env as_type exp =
  let rec exp_typer = function
  | Unit -> false
  | Var path -> (* TODO *) false
  | ConstInt i -> check_int_coercion i as_type
  | ConstInt32 i -> check_int32_coercion i as_type
  | ConstInt64 i -> check_int64_coercion i as_type
  | Apply (fname, arglist) -> (* TODO *) false
  in
    exp_typer exp
