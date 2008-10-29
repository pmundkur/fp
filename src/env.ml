type t = {
  (* Functions and types are currently predefined only. *)
  functions: Types.function_info Ident.env;
  types: Types.type_info Ident.env;

  (* Variants and formats are defined at the top-level. *)
  variants: Types.variant_info Ident.env;
  formats: Types.format_info Ident.env;

  (* Fields need a dynamic stacked environment. *)
  fields: Types.field_info Ident.env;
}

let new_env () =
  { types      = Ident.empty_env;
    functions  = Ident.empty_env;
    variants   = Ident.empty_env;
    formats    = Ident.empty_env;
    fields     = Ident.empty_env;
  }

let all_fields_by_id = ref Ident.empty_env

let lookup_field_by_name t n =
  Ident.assoc_by_name t.fields n

let lookup_field_by_id t i =
  Ident.assoc_by_id t.fields i

let lookup_variant_by_name t n =
  Ident.assoc_by_name t.variants n

let lookup_variant_by_id t i =
  Ident.assoc_by_id t.variants i

let lookup_format_by_name t n =
  Ident.assoc_by_name t.formats n


let lookup_format_by_id t i =
  Ident.assoc_by_id t.formats i

let lookup_function_by_name t n =
  Ident.assoc_by_name t.functions n

let lookup_function_by_id t i =
  Ident.assoc_by_id t.functions i

let lookup_type_by_name t n =
  Ident.assoc_by_name t.types n

let lookup_type_by_id t i =
  Ident.assoc_by_id t.types i


let global_lookup_field_by_id t i =
  Ident.assoc_by_id !all_fields_by_id i


let add_type tid tinfo env =
  { env with
      types = Ident.add tid tinfo env.types }

let add_function fid finfo env =
  { env with
      functions = Ident.add fid finfo env.functions }

let add_variant_def vid vinfo env =
  { env with
      variants = Ident.add vid vinfo env.variants }

let add_field fid finfo env =
  all_fields_by_id := Ident.add fid finfo !all_fields_by_id;
  { env with
      fields = Ident.add fid finfo env.fields }

let add_format_def fid finfo env =
  { env with
      formats = Ident.add fid finfo env.formats }


let extract_field_env env =
  env.fields
