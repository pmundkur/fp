type t

val new_env: unit -> t

(* Lookups by name also return the identifier.  Field lookups also return the nesting depth. *)
val lookup_field_by_name: t -> string -> ((Ident.t * Types.field_info) * int) option
val lookup_variant_by_name: t -> string -> (Ident.t * Types.variant_info) option
val lookup_format_by_name: t -> string -> (Ident.t * Types.format_info) option
val lookup_function_by_name: t -> string -> (Ident.t * Types.function_info) option
val lookup_type_by_name: t -> string -> (Ident.t * Types.type_info) option

val lookup_field_by_id: t -> Ident.t -> (Types.field_info * int) option
val lookup_variant_by_id: t -> Ident.t -> Types.variant_info option
val lookup_format_by_id: t -> Ident.t -> Types.format_info option
val lookup_function_by_id: t -> Ident.t -> Types.function_info option
val lookup_type_by_id: t -> Ident.t -> Types.type_info option

val add_type: string -> Types.type_info -> t -> t
val add_function: string -> Types.function_info -> t -> t
val add_variant_def: string -> Types.variant_info -> t -> t
