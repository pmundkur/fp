type 'a stacked_env =
  | Env_top
  | Env_frame of (Ident.t * 'a) list * ('a stacked_env)

type t = {
  (* Functions are currently predefined only. *)
  functions : (Ident.t * Types.function_info) list;

  (* Variants and formats are defined at the top-level. *)
  variants : (Ident.t * Types.variant_info) list;
  formats  : (Ident.t * Types.format_info)  list;

  (* Fields need a dynamic stacked environment. *)
  fields   : Types.field_info stacked_env;
}

let new_env () =
  { functions = [];
    variants  = [];
    formats   = [];
    fields    = Env_top }

let assoc_match match_fn ret_fn frame n =
  let rec assc l =
    match l with
      | [] ->
          None
      | (i, info) :: tl ->
          if match_fn i n then ret_fn i info
          else assc tl
  in
    assc frame

let assoc_by_name frame n =
  assoc_match
    (fun i n -> Ident.name_of i = n)
    (fun i info -> Some (i, info))
    frame n

let assoc_by_id frame i =
  assoc_match
    (fun i n -> Ident.compare i n = 0)
    (fun i info -> Some info)
    frame i

let lookup_field assoc_fn env sym =
  let rec lookup_with_nesting env' depth =
    match env' with
      | Env_top -> None
      | Env_frame (frame, next) ->
          match assoc_fn frame sym with
            | Some info ->
                Some (info, depth)
            | None ->
                lookup_with_nesting next (depth + 1)
  in
    lookup_with_nesting env 0

let lookup_field_by_name t n =
  lookup_field assoc_by_name t.fields n

let lookup_field_by_id t i =
  lookup_field assoc_by_id t.fields i

let lookup_variant_by_name t n =
  assoc_by_name t.variants n

let lookup_variant_by_id t i =
  assoc_by_id t.variants i

let lookup_format_by_name t n =
  assoc_by_name t.formats n

let lookup_format_by_id t i =
  assoc_by_id t.formats i

let lookup_function_by_name t n =
  assoc_by_name t.functions n

let lookup_function_by_id t i =
  assoc_by_id t.functions i

let add_function fn finfo env =
  let ident = Ident.make_ident fn in
    { env with
        functions = (ident, finfo) :: env.functions }
