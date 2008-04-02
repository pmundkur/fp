type t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position
}

let init lexbuf fname =
  lexbuf.lex_curr_p <-
    { pos_fname = fname;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0 }
