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

