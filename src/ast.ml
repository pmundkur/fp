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
  | Pvariant of decl_name * variant
  | Pformat of decl_name * format

and variant = (exp * case_name * default) list

and format = field list

and field =
  | Pnamed_field of field_name * field_type
  | Palign of exp

and field_type =
  | Psimple of type_exp * field_attrib list
  | Parray of exp * format
  | Pclassify of exp * classify_case list
  | Plabel

and field_attrib =
  | Pmax of exp
  | Pmin of exp
  | Pconst of exp
  | Pdefault  of exp
  | Pvalue of exp
  | Pvariant_ref of decl_name
  | Pvariant_inline of variant

and type_exp =
  | Pbase of type_name
  | Pvector of type_name * exp

and classify_case = case_name * case_exp * format

and case_exp =
  | Pcase_const of exp
  | Pcase_range of exp * exp

and exp =
  | Punit
  | Pvar of path
  | Pconst_int of int
  | Pconst_int32 of Int32.t
  | Pconst_int64 of Int64.t
  | Papply of fun_name * exp list

and path =
  | Pfield of field_name
  | Ppath of field_name * case_name * path

