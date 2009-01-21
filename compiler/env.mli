type t

val new_env: unit -> t

(* Lookups by name also return the identifier.  Field lookups also return the nesting depth. *)
val lookup_field_by_name: t -> string -> (Ident.t * Types.field_type) option
val lookup_variant_by_name: t -> string -> (Ident.t * Types.variant_info) option
val lookup_format_by_name: t -> string -> (Ident.t * Types.format_info) option
val lookup_function_by_name: t -> string -> (Ident.t * Types.function_info) option
val lookup_type_by_name: t -> string -> (Ident.t * Types.type_info) option

val lookup_field_by_id: t -> Ident.t -> Types.field_type option
val lookup_variant_by_id: t -> Ident.t -> Types.variant_info option
val lookup_format_by_id: t -> Ident.t -> Types.format_info option
val lookup_function_by_id: t -> Ident.t -> Types.function_info option
val lookup_type_by_id: t -> Ident.t -> Types.type_info option

val global_lookup_field_by_id: t -> Ident.t -> Types.field_type option
val get_formats: t -> Types.format_info Ident.env

val add_type: Ident.t -> Types.type_info -> t -> t
val add_function: Ident.t -> Types.function_info -> t -> t
val add_variant_def: Ident.t -> Types.variant_info -> t -> t
val add_field: Ident.t -> Types.field_type -> t -> t
val add_format_def: Ident.t -> Types.struct_type -> t -> t

(* Note that path lookup is by Ast.path, while paths are added using Types.path *)
val lookup_path: t -> Ast.path -> (Types.path * Types.struct_type) option
val add_path: Types.path -> Asttypes.case_name -> Types.struct_type -> t -> t
val get_paths: t -> (Types.path * Asttypes.case_name * Types.struct_type) list

val extract_field_env: t -> Types.field_type Ident.env

(* These are used to implement the multiple use check for classification *)
val add_branch_field: Ident.t -> Location.t -> t -> t
val find_branch_field: t -> Ident.t -> Location.t option

(* This is used to get a new typing environment scrubbed free of all
   transient state. *)
val clone: t -> t
