%{
  open Ast
  (* helpers to process tokens from the lexer *)
  let loc = fst
  let carrier = snd
  let token_to_located_node tok =
    Location.make_located_node (carrier tok) (loc tok)

  let raise_parse_error e loc =
    raise (Parsing_error (e, loc))
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
    { Variant ((token_to_located_node $3), $4) }
| FORMAT LCID format
    { Format ((token_to_located_node $2), $3) }
;

variant:
| LCURLY variant_cases RCURLY
    { List.rev $2 }
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
    { List.rev $2 }
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
    { Align $2 }
| LCID COLON field_type
    { Named_field ((token_to_located_node $1), $3) }
;

field_type:
| CLASSIFY LPAREN LCID RPAREN LCURLY cases RCURLY
    { let e = Var (Field (token_to_located_node $3)) in
        Classify (e, (List.rev $6))
    }
| ARRAY LPAREN exp RPAREN format
    { Array ($3, $5) }
| type_exp field_attribs
    { Simple ($1, (List.rev $2)) }
| LABEL
    { Label }
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
        case_name, case_exp, [ $3 ]
    }
;

case_exp:
| BAR exp DOTDOT exp COLON UCID
    { (token_to_located_node $6), (Case_range ($2, $4)) }
| BAR exp COLON UCID
    { (token_to_located_node $4), (Case_const $2) }
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
        match carrier $1 with
          | "max" -> Max e
          | "min" -> Min e
          | "const" -> Const e
          | "default" -> Default e
          | "value" -> Value e
          |  n -> raise_parse_error (Unknown_field_attribute n) (loc $1)
    }
| VARIANT LCID
    { VariantRef (token_to_located_node $2) }
| VARIANT variant
    { VariantInline $2 }
;

exp:
| INT
    { ConstInt (carrier $1) }
| INT32
    { ConstInt32 (carrier $1) }
| INT64
    { ConstInt64 (carrier $1) }
| path
    { Var $1 }
| LCID LPAREN exp_list RPAREN
    { Apply ((token_to_located_node $1), (List.rev $3)) }
| exp PLUS exp
    { Apply ((Location.make_located_node  "+" $2), [$1; $3]) }
| exp MINUS exp
    { Apply ((Location.make_located_node  "-" $2), [$1; $3]) }
| exp TIMES exp
    { Apply ((Location.make_located_node  "*" $2), [$1; $3]) }
| exp DIV exp
    { Apply ((Location.make_located_node  "/" $2), [$1; $3]) }
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
    { Field (token_to_located_node $1) }
| LCID LSQUARE UCID RSQUARE DOT path
    { Path ((token_to_located_node $1), (token_to_located_node $3), $6) }
;

type_exp:
| LCID LSQUARE exp RSQUARE
    { Vector ((token_to_located_node $1), $3) }
| LCID
    { Base (token_to_located_node $1) }
;

opt_semi:
| SEMI
    {}
| /* epsilon */
    {}
