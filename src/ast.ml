type syntax_error =
  | Illegal_character of char
  | Unmatched_comment
  | Unterminated_comment

exception Syntax_error of syntax_error * Lexing.position

type parse_error =
  | Unknown_operator of string
  | Unknown_field_attribute of string

exception Parsing_error of parse_error * Location.t

type default = bool

type decl_name = string Location.located_node

type field_name = string Location.located_node

type case_name = string Location.located_node

type fun_name = string Location.located_node

type type_name = string Location.located_node

type label_name = string Location.located_node

type decl =
  | Variant of decl_name * variant
  | Format of decl_name * format

and variant = (exp * case_name * default) list

and format = field list

and field =
  | Named_field of field_name * field_type
  | Align of exp

and field_type =
  | Simple of type_exp * field_attrib list
  | Array of exp * format
  | Classify of exp * classify_case list
  | Label

and field_attrib =
  | Max of exp
  | Min of exp
  | Const of exp
  | Default  of exp
  | Value of exp
  | VariantRef of decl_name
  | VariantInline of variant

and type_exp =
  | Base of type_name
  | Vector of type_name * exp

and classify_case = case_name * exp * format

and exp =
  | Unit
  | Var of path
  | ConstInt of int
  | ConstInt32 of Int32.t
  | ConstInt64 of Int64.t
  | Fun of fun_name * exp list

and path =
  | Root of field_name
  | Path of path * case_name * field_name

