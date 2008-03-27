open Sp_lib
open Pcap

exception Parse_error of string

let print_usage argv0 =
  Printf.eprintf "Usage: %s file\n" argv0;
  exit 1

let setup_env fname =
  let fin = open_in fname in
  let fin_size = in_channel_length fin in
  let buf = String.make fin_size '\000' in
  really_input fin buf 0 fin_size;
    { Env.buf = buf;
      Env.blen = fin_size;
      Env.vstart = 0;
      Env.vlen = fin_size;
      Env.start_bit = 0;
      Env.end_bit = 7;
      Env.endian = Env.Little_endian }

let pcap_parse env =
  let magic_t, cursor = SP_int32.unmarshal env in
    Env.pretty_print cursor;
    let e =
      (match SP_int32.read magic_t with
         | 0xa1b2c3d4l -> Env.as_little_endian cursor
         | 0xd4c3b2a1l -> Env.as_big_endian cursor
         | magic -> raise (Parse_error (Printf.sprintf "Unknown pcap magic %lx" magic))) in
      Env.pretty_print e;
  let hdr, e = Pcap_header.unmarshal e in
    Pcap_header.pretty_print hdr;
    let eref = ref e in
    let cnt = ref 0 in
    while (Env.is_non_empty !eref)  do
      let entry, e = Pcap_entry.unmarshal !eref in
        Printf.printf "%d:\n" !cnt;
        Pcap_entry.pretty_print entry;
        eref := e;
        incr cnt
    done

let main () =
  let argc = Array.length Sys.argv in
    if argc != 2 then
      print_usage Sys.argv.(0);
    let e = setup_env Sys.argv.(1) in
      pcap_parse e;;

main ()
