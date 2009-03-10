(**************************************************************************)
(*  Copyright 2009          Prashanth Mundkur.                            *)
(*  Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>          *)
(*                                                                        *)
(*  This file is part of FormatCompiler.                                  *)
(*                                                                        *)
(*  FormatCompiler is free software: you can redistribute it and/or       *)
(*  modify it under the terms of the GNU Affero General Public            *)
(*  License as published by the Free Software Foundation, either          *)
(*  version 3 of the License, or (at your option) any later version.      *)
(*                                                                        *)
(*  FormatCompiler is distributed in the hope that it will be useful,     *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with FormatCompiler.  If not, see                       *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

open Lexing
open Asttypes

let options = [ ("-test", Arg.Set Config.test_mode, " unit-test mode") ]

let parse_args () =
  let files = ref [] in
  let usage = Printf.sprintf "Usage: %s [options] files" Sys.argv.(0) in
    Arg.parse (Arg.align options) (fun f -> files := f :: !files) usage;
    List.rev !files

let handle_syntax_error e l =
  let msg =
    match e with
      | Illegal_character c -> Printf.sprintf "Illegal character %c" c
      | Unmatched_comment -> Printf.sprintf "Unmatched comment"
      | Unterminated_comment -> Printf.sprintf "Unterminated comment"
  in
    Printf.printf "%s at file \"%s\", line %d, character %d\n"
      msg l.pos_fname l.pos_lnum (l.pos_cnum - l.pos_bol);
    Util.exit_with_code 1

let handle_parse_error lexbuf =
  Printf.printf "Parse error in %s at line %d, char %d\n"
    lexbuf.lex_curr_p.pos_fname lexbuf.lex_curr_p.pos_lnum
    (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
  Util.exit_with_code 1

let handle_parsing_error e l =
  let st = Location.start_of l in
  let ed = Location.end_of l in
  let msg =
    match e with
      | Unknown_operator s -> Printf.sprintf "Unknown operator %s" s
      | Unknown_field_attribute s -> Printf.sprintf "Unknown field attribute %s" s
  in
    (if st.pos_lnum = ed.pos_lnum then
       Printf.printf "%s at file \"%s\", line %d, characters %d-%d\n"
         msg st.pos_fname st.pos_lnum (st.pos_cnum - st.pos_bol)
         (ed.pos_cnum - ed.pos_bol)
     else
       Printf.printf "%s at file \"%s\", line %d, character %d - line %d, character %d\n"
         msg st.pos_fname st.pos_lnum (st.pos_cnum - st.pos_bol)
         ed.pos_lnum (ed.pos_cnum - ed.pos_bol));
    Util.exit_with_code 1

let parse_file f ic =
  let lexbuf = Lexing.from_channel ic in
  let _ = Lexer.init lexbuf f in
    try
      Parser.spec Lexer.main lexbuf
    with
      | Syntax_error (e, l) ->
          handle_syntax_error e l
      | Parsing.Parse_error ->
          handle_parse_error lexbuf
      | Parsing_error (e, l) ->
          handle_parsing_error e l

let pprint pt =
  let ff = Format.formatter_of_out_channel stdout in
    List.iter (Ast.pr_decl ff) pt;
    Format.fprintf ff "\n"

let process_file f =
  try
    let ic = open_in f in
    let pt = parse_file f ic in
    (* let _ = pprint pt in *)
    let typed_env = Typing.type_check (Typing.init_typing_env ()) pt in
    let formats = Env.get_formats typed_env in
      Patternmatch.check_formats formats;
      ignore (Dependency.analyze_formats formats)
  with
    | Sys_error s ->
        prerr_endline s;
        Util.exit_with_code 1
    | e ->
        Printf.fprintf stderr "%s\n" (Printexc.to_string e);
        Util.exit_with_code 1

let _ =
  let input_files = parse_args () in
    List.iter (fun f -> process_file f) input_files
