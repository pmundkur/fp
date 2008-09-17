%{
  open Asttypes
  open Ast
  (* helpers to process tokens from the lexer *)
  let loc = fst
  let carrier = snd
  let token_to_located_node tok =
    Location.make_located_node (carrier tok) (loc tok)

  let raise_parse_error e loc =
    raise (Parsing_error (e, loc))

  let mk_decl d loc =
    { pdecl_desc = d; pdecl_loc = loc }
  let mk_variant v loc =
    { pvariant_desc = v; pvariant_loc = loc }
  let mk_format f loc =
    { pformat_desc = f; pformat_loc = loc }
  let mk_field f loc =
    { pfield_desc = f; pfield_loc = loc }
  let mk_field_type ft loc =
    { pfield_type_desc = ft; pfield_type_loc = loc }
  let mk_field_attrib fa loc =
    { pfield_attrib_desc = fa; pfield_attrib_loc = loc }
  let mk_type_exp te loc =
    { ptype_exp_desc = te; ptype_exp_loc = loc }
  let mk_case_exp ce loc =
    { pcase_exp_desc = ce; pcase_exp_loc = loc }
  let mk_exp e loc =
    { pexp_desc = e; pexp_loc = loc }

  let mk_with_rloc mk d =
      mk d (Location.symbol_rloc ())
%}

%token <Location.t> DEF ARRAY ALIGN LABEL FORMAT VARIANT CLASSIFY

%token <Location.t> LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token <Location.t> DOT DOTDOT COMMA SEMI COLON BAR ARROW DEFARROW
%token <Location.t> EOF

%token <Location.t * string> UCID LCID
%token <Location.t> PLUS MINUS TIMES DIV
%left PLUS MINUS
%left TIMES DIV


%token <Location.t * int> INT
%token <Location.t * Int32.t> INT32
%token <Location.t * Int64.t> INT64

%start spec
%type <Ast.decl list> spec

%%

spec:
| decls EOF
    { List.rev $1 }
;

decls:
| decls decl
    { $2 :: $1 }
| /* epsilon */
    { [] }
;

decl:
| DEF VARIANT LCID variant
    { mk_with_rloc mk_decl (Pdecl_variant ((token_to_located_node $3), $4)) }
| FORMAT LCID format
    { mk_with_rloc mk_decl (Pdecl_format ((token_to_located_node $2), $3)) }
;

variant:
| LCURLY variant_cases RCURLY
    { mk_with_rloc mk_variant (List.rev $2) }
;

variant_cases:
| variant_case
    { [ $1 ] }
| variant_cases variant_case
    { $2 :: $1 }
;

variant_case:
| BAR exp ARROW UCID
    { ($2, (token_to_located_node $4), false) }
| BAR exp DEFARROW UCID
    { ($2, (token_to_located_node $4), true) }
;

format:
| LCURLY fields RCURLY
    { mk_with_rloc mk_format (List.rev $2) }
;

fields:
| field_list opt_semi
    { $1 }
| /* epsilon */
    { [] }

field_list:
| field_list SEMI field
    { $3 :: $1 }
| field
    { [ $1 ] }
;

field:
| ALIGN exp
    { mk_with_rloc mk_field (Pfield_align $2) }
| LCID COLON field_type
    { mk_with_rloc mk_field (Pfield_name ((token_to_located_node $1), $3)) }
;

field_type:
| CLASSIFY LPAREN LCID RPAREN LCURLY cases RCURLY
    { let loc = Location.rhs_loc 3 in
      let e = mk_exp (Pexp_var (Pfield (token_to_located_node $3))) loc in
        mk_with_rloc mk_field_type (Ptype_classify (e, (List.rev $6)))
    }
| ARRAY LPAREN exp RPAREN format
    { mk_with_rloc mk_field_type (Ptype_array ($3, $5)) }
| type_exp field_attribs
    { mk_with_rloc mk_field_type (Ptype_simple ($1, (List.rev $2))) }
| LABEL
    { mk_with_rloc mk_field_type (Ptype_label) }
;

cases:
| cases case
    { $2 :: $1 }
| /* epsilon */
    { [] }
;

case:
| case_exp ARROW format
    { let case_name, case_exp = $1 in
        case_name, case_exp, $3
    }
| case_exp ARROW field
    { let case_name, case_exp = $1 in
        case_name, case_exp, (mk_format [ $3 ]
                                (Location.rhs_loc 3))
    }
;

case_exp:
| BAR exp DOTDOT exp COLON UCID
    { let lloc, rloc  = Location.rhs_loc 2, Location.rhs_loc 4 in
      let loc = Location.span lloc rloc in
      (token_to_located_node $6), mk_case_exp (Pcase_range ($2, $4)) loc }
| BAR exp COLON UCID
    { let loc = Location.rhs_loc 2 in
      (token_to_located_node $4), mk_case_exp (Pcase_const $2) loc }
;

field_attribs:
| field_attribs field_attrib
    { $2 :: $1 }
| /* epsilon */
    { [] }
;

field_attrib:
| LCID LPAREN exp RPAREN
    { let e = $3 in
      let fa =
        match carrier $1 with
          | "max" -> Pattrib_max e
          | "min" -> Pattrib_min e
          | "const" -> Pattrib_const e
          | "default" -> Pattrib_default e
          | "value" -> Pattrib_value e
          |  n -> raise_parse_error (Unknown_field_attribute n) (loc $1)
      in
        mk_with_rloc mk_field_attrib fa
    }
| VARIANT LCID
    { mk_with_rloc mk_field_attrib (Pattrib_variant_ref (token_to_located_node $2)) }
| VARIANT variant
    { mk_with_rloc mk_field_attrib (Pattrib_variant_inline $2) }
;

exp:
| INT
    { mk_with_rloc mk_exp (Pexp_const_int (carrier $1)) }
| INT32
    { mk_with_rloc mk_exp (Pexp_const_int32 (carrier $1)) }
| INT64
    { mk_with_rloc mk_exp (Pexp_const_int64 (carrier $1)) }
| path
    { mk_with_rloc mk_exp (Pexp_var $1) }
| LCID LPAREN exp_list RPAREN
    { mk_with_rloc mk_exp (Pexp_apply ((token_to_located_node $1), (List.rev $3))) }
| exp PLUS exp
    { mk_with_rloc mk_exp (Pexp_apply ((Location.make_located_node  "+" $2), [$1; $3])) }
| exp MINUS exp
    { mk_with_rloc mk_exp (Pexp_apply ((Location.make_located_node  "-" $2), [$1; $3])) }
| exp TIMES exp
    { mk_with_rloc mk_exp (Pexp_apply ((Location.make_located_node  "*" $2), [$1; $3])) }
| exp DIV exp
    { mk_with_rloc mk_exp (Pexp_apply ((Location.make_located_node  "/" $2), [$1; $3])) }
| LPAREN exp RPAREN
    { $2 }
;

exp_list:
| exp_list COMMA exp
    { $3 :: $1 }
| exp
    { [ $1 ] }
| /* epsilon */
    { [] }
;

path:
| LCID
    { Pfield (token_to_located_node $1) }
| LCID LSQUARE UCID RSQUARE DOT path
    { Ppath ((token_to_located_node $1), (token_to_located_node $3), $6) }
;

type_exp:
| LCID LSQUARE exp RSQUARE
    { mk_with_rloc mk_type_exp (Pvector ((token_to_located_node $1), $3)) }
| LCID
    { mk_with_rloc mk_type_exp (Pbase (token_to_located_node $1)) }
;

opt_semi:
| SEMI
    {}
| /* epsilon */
    {}
