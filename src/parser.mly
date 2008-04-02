%{
  module L = Location

  let loc = fst

  let carrier = snd

%}

%token <Location.t> DEF ARRAY ALIGN STRUCT VARIANT CLASSIFY

%token <Location.t> LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token <Location.t> DOT COMMA SEMI COLON BAR ARROW DEFARROW
%token <Location.t> EOF

%token <Location.t * string> UCID LCID
%left PLUS MINUS
%left TIMES DIV

%token <Location.t * int> INT
%token <Location.t * Int32.t> INT32
%token <Location.t * Int64.t> INT64

%start spec
%type <Ast.located_decl list> spec

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
    {}
| STRUCT LCID format
    {}
;

variant:
| LCURLY variant_cases RCURLY
    {}
;

variant_cases:
| variant_case
    {}
| variant_cases SEMI variant_case
    {}
;

variant_case:
| BAR exp ARROW UCID
    {}
| BAR exp DEFARROW UCID
    {}
;

format:
| LCURLY fields RCURLY
    {}
;

fields:
| fields field
    {}
| /* epsilon */
    {}
;

field:
| ALIGN exp
    {}
| LCID COLON field_type
    {}
;

field_type:
| type_exp SEMI field_attribs
  {}
| CLASSIFY LPAREN LCID RPAREN cases
    {}
| ARRAY LPAREN exp RPAREN format
    {}
;

cases:
| cases case
    {}
| /* epsilon */
    {}

case:
| BAR exp COLON LCID ARROW format
    {}

field_attribs:
| LCID LPAREN exp RPAREN
      {}
| VARIANT LCID
    {}
| VARIANT variant
    {}
;

exp:
| LCID LPAREN exp_list RPAREN
    {}
| exp PLUS exp
    {}
| exp MINUS exp
    {}
| exp TIMES exp
    {}
| exp DIV exp
    {}
| LPAREN exp RPAREN
    {}
| path DOT LCID
    {}
| LCID
    {}
;

exp_list:
| exp COMMA exp_list
    {}
| exp
    {}
| /* epsilon */
    {}
;

path:
| LCID
    {}
| path LSQUARE LCID RSQUARE DOT LCID
    {}

type_exp:
| LCID LSQUARE exp RSQUARE
    { }
| LCID
    { let id = carrier $1 in
      let l = loc $1 in
	Base (located_nod_of id l) }
;

