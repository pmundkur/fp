(**************************************************************************)
(*  Copyright 2009-2013       Prashanth Mundkur.                          *)
(*  Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>          *)
(*                                                                        *)
(*  This file is part of FormatCompiler.                                  *)
(*                                                                        *)
(*  FormatCompiler is free software: you can redistribute it and/or       *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation, either          *)
(*  version 3 of the License, or (at your option) any later version.      *)
(*                                                                        *)
(*  Alternatively, this software may be distributed, used, and modified   *)
(*  under the terms of the BSD license.                                   *)
(*                                                                        *)
(*  FormatCompiler is distributed in the hope that it will be useful,     *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(**************************************************************************)

{
  open Lexing
  open Parser
  open Asttypes

  let raise_syntax_error e pos =
    raise (Syntax_error (e, pos))

  let raise_parse_error e loc =
    raise (Parsing_error (e, loc))

  let keyword_list = [
    ("array", fun l -> ARRAY l);
    ("align", fun l -> ALIGN l);
    ("value", fun l -> VALUE l);
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
    ("..", fun l -> DOTDOT l);
    (",", fun l -> COMMA l);
    (";", fun l -> SEMI l);
    (":", fun l -> COLON l);
    ("|", fun l -> BAR l);
    ("->", fun l -> ARROW l);
    ("=>", fun l -> DEFARROW l);
    ("=", fun l -> EQUAL l);

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
      if str = "_" then
        UNDERSCORE loc
      else match String.get str 0 with
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

let dec_literal = ['0'-'9']+
let hex_literal = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let num_literal = dec_literal | hex_literal
let int32_literal = num_literal "l"
let int64_literal = num_literal "L"
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
  | num_literal
      { make_int (locate lexbuf) (lexeme lexbuf) }
  | ident
      { lookup_id (locate lexbuf) (lexeme lexbuf) }
  | "{" | "}" | "(" | ")" | "[" | "]" | "." | ".." | "," | ";" | ":" | "|" | "->" | "=>" | "=" | "+" | "-" | "*" | "/"
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
  | ['\n']
      { newline lexbuf; comment lexbuf }
  | eof
      { raise_syntax_error Unterminated_comment !comment_start }
  | _
      { comment lexbuf }
