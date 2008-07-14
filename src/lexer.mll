{
  open Lexing
  open Parser
  open Ast

  let raise_syntax_error e pos =
    raise (Syntax_error (e, pos))

  let raise_parse_error e loc =
    raise (Parsing_error (e, loc))

  let keyword_list = [
    ("def", fun l -> DEF l);
    ("array", fun l -> ARRAY l);
    ("align", fun l -> ALIGN l);
    ("label", fun l -> LABEL l);
    ("format", fun l -> FORMAT l);
    ("variant", fun l -> VARIANT l);
    ("classify", fun l -> CLASSIFY l);

    ("{", fun l -> LCURLY l);
    ("}", fun l -> RCURLY l);
    ("(", fun l -> LPAREN l);
    (")", fun l -> RPAREN l);
    ("[", fun l -> LSQUARE l);
    ("]", fun l -> RSQUARE l);
    (".", fun l -> DOT l);
    (",", fun l -> COMMA l);
    (";", fun l -> SEMI l);
    (":", fun l -> COLON l);
    ("|", fun l -> BAR l);
    ("->", fun l -> ARROW l);
    ("=>", fun l -> DEFARROW l);

    ("+", fun l -> PLUS l);
    ("-", fun l -> MINUS l);
    ("*", fun l -> TIMES l);
    ("/", fun l -> DIV l);
  ]

  let (keyword_table : (string, (Location.t -> token)) Hashtbl.t) = Hashtbl.create 10
  let _ = List.iter (fun (str, f) -> Hashtbl.add keyword_table str f) keyword_list

  let lookup_id loc str =
    try (Hashtbl.find keyword_table str) loc
    with Not_found ->
      match String.get str 0 with
        | 'A' .. 'Z' -> UCID (loc, str)
        | 'a' .. 'z' -> LCID (loc, str)
        | _ ->          raise_parse_error (Unknown_operator str) loc

  let depth = ref 0
  let comment_start = ref dummy_pos

  let init lexbuf fname =
    lexbuf.lex_curr_p <-
      { pos_fname = fname;
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0 }

  let newline lexbuf =
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                             pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
                             pos_bol = lexbuf.lex_curr_p.pos_cnum }

  let locate lexbuf =
    Location.make_location (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)

  let trim_suffix lex n =
    String.sub lex 0 ((String.length lex) - n)

  let make_int loc lex =
      INT (loc, (int_of_string lex))

  let make_int32 loc lex =
    let s = trim_suffix lex 1 in
      INT32 (loc, (Int32.of_string s))

  let make_int64 loc lex =
    let s = trim_suffix lex 1 in
      INT64 (loc, (Int64.of_string s))
}

let int_literal = ['0'-'9']+
let int32_literal = ['0'-'9']+ "l"
let int64_literal = ['0'-'9']+ "L"
let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*

rule main = parse
  | [' ' '\009' '\012' '\r']+
      { main lexbuf }
  | ['\n']
      { newline lexbuf; main lexbuf }
  | "*/"
      { raise_syntax_error Unmatched_comment (lexeme_start_p lexbuf) }
  | "/*"
      { depth := 1; comment_start := lexeme_start_p lexbuf;
        comment lexbuf; main lexbuf }
  | int64_literal
      { make_int64 (locate lexbuf) (lexeme lexbuf) }
  | int32_literal
      { make_int32 (locate lexbuf) (lexeme lexbuf) }
  | int_literal
      { make_int (locate lexbuf) (lexeme lexbuf) }
  | ident
      { lookup_id (locate lexbuf) (lexeme lexbuf) }
  | "()"
      { UNIT (locate lexbuf) }
  | "{" | "}" | "(" | ")" | "[" | "]" | ";" | ":" | "|" | "->" | "=>" | "+" | "-" | "*" | "/"
      { lookup_id (locate lexbuf) (lexeme lexbuf) }
  | eof
      { EOF (locate lexbuf) }
  | _
      { raise_syntax_error (Illegal_character (lexeme_char lexbuf 0)) (lexeme_start_p lexbuf) }

and comment = parse
  | "/*"
      { depth := succ !depth; comment lexbuf }
  | "*/"
      { depth := pred !depth; if !depth > 0 then comment lexbuf }
  | eof
      { raise_syntax_error Unterminated_comment !comment_start }
  | _
      { comment lexbuf }
