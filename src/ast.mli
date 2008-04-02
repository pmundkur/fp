type 'a located_nod

val nod_of : 'a located_nod -> 'a

val location_of : 'a located_nod -> Location.t

val located_nod_of : 'a -> Location.t -> 'a located_nod

type default = string option

type decl_name = string located_nod

type field_name = string located_nod

type case_name = string located_nod

type fun_name = string located_nod

type type_name = string located_nod

type path =
  | Root of field_name
  | Path of path * case_name * field_name

type decl =
  | Variant of decl_name * variant located_nod
  | Format of decl_name * format located_nod

and located_decl = decl located_nod

and variant = (exp * case_name * default) list

and exp =
  | Var of path
  | ConstInt of int
  | ConstInt32 of Int32.t
  | ConstInt64 of Int64.t
  | Fun of fun_name * (exp located_nod) list

and field_attrib =
  | Max of exp located_nod
  | Min of exp located_nod
  | Const of exp located_nod
  | Default  of exp located_nod
  | Value of exp located_nod

and type_exp =
  | Base of type_name
  | Vector of type_name * exp located_nod

and field =
  | Named_field of field_name * field_type located_nod
  | Align of exp located_nod

and field_type =
  | Simple of type_exp located_nod * field_attrib list
  | Array of exp located_nod * format located_nod
  | Classify of exp located_nod * classify_case list
  | Label

and classify_case = case_name * exp located_nod * format located_nod

and format = (field located_nod) list
