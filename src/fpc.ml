
let options = []

let parse_args () =
  let files = ref [] in
  let usage = Printf.sprintf "Usage: %s [options] files" Sys.argv.(0) in
    Arg.parse (Arg.align options) (fun f -> files := f :: !files) usage;
    List.rev !files

let parse_file f ic =
  let lexbuf = Lexing.from_channel ic in
  let _ = Lexer.init lexbuf f in
  let spec = Parser.spec Lexer.main lexbuf in
    spec

let process_file f =
  try
    let ic = open_in f in
      ignore (parse_file f ic)
  with
      Sys_error s -> prerr_endline s

let _ =
  let input_files = parse_args () in
    List.iter (fun f -> process_file f) input_files

