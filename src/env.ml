type 'a stacked_env =
  | Env_top
  | Env_frame of (Ident.t * 'a) list * ('a stacked_env)

type t = {
  (* Functions and types are currently predefined only. *)
  functions: (Ident.t * Types.function_info) list;
  types: (Ident.t * Types.type_info) list;

  (* Variants and formats are defined at the top-level. *)
  variants: (Ident.t * Types.variant_info) list;
  formats: (Ident.t * Types.format_info)  list;

  (* Fields need a dynamic stacked environment. *)
  fields: (Ident.t * Types.field_info) list;
}

let new_env () =
  { types     = [];
    functions = [];
    variants  = [];
    formats   = [];
    fields    = [] }

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

(*let lookup_field assoc_fn env sym =
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
*)

let lookup_field_by_name t n =
  assoc_by_name t.fields n

let lookup_field_by_id t i =
  assoc_by_id t.fields i

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

let lookup_type_by_name t n =
  assoc_by_name t.types n

let lookup_type_by_id t i =
  assoc_by_id t.types i

let add_type tid tinfo env =
  { env with
      types = (tid, tinfo) :: env.types }

let add_function fid finfo env =
  { env with
      functions = (fid, finfo) :: env.functions }

let add_variant_def vid vinfo env =
  { env with
      variants = (vid, vinfo) :: env.variants }

let add_field fid finfo env =
  { env with
      fields = (fid, finfo) :: env.fields }
