open Lexing

type t = {
  loc_start : position;
  loc_end : position
}

let init lexbuf fname =
  lexbuf.lex_curr_p <-
    { pos_fname = fname;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0 }
