(**************************************************************************)
(*  Copyright 2009          Prashanth Mundkur.                            *)
(*  Author  Prashanth Mundkur <prashanth.mundkur@gmail.com>               *)
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

exception Arg_out_of_bounds

module Env = struct

  exception Invalid_access of string
  exception Invalid_op of string
  exception Insufficient_data of string

  type endian =
    | Big_endian
    | Little_endian

  type t = {
    buf: string;        (* data buffer *)
    blen: int;          (* length of valid data in buffer *)
    vstart: int;        (* start of view into buffer *)
    vlen: int;          (* length of view into buffer *)
    start_bit: int;     (* bit offset into first byte *)
    end_bit: int;       (* bit offset into last byte *)
    endian: endian      (* endianness of data view *)
  }

  let pretty_print env =
    let p = print_endline in
      p (" [Env.t]");
      p (" bufsize = " ^ (string_of_int (String.length env.buf)));
      p (" buflen = " ^ (string_of_int env.blen));
      p (" vstart = " ^ (string_of_int env.vstart));
      p (" vlen = " ^ (string_of_int env.vlen));
      p (" vend = " ^ (string_of_int (env.vstart + env.vlen)));
      p (" start_bit = " ^ (string_of_int env.start_bit));
      p (" end_bit = " ^ (string_of_int env.end_bit));
      p (" endian = " ^ (match env.endian with Big_endian -> "Big" | _ -> "Little"))

  type byte_data = [
  | `None
  | `Str of string
  | `Frag of t
  | `Fill of (t -> t * t)   (* returns view of filled-in data + next view *)
  ]

  let endian env =
    env.endian

  let is_big_endian env =
    match env.endian with
      | Big_endian -> true
      | Little_endian -> false

  let is_little_endian env =
    not (is_big_endian env)

  let as_big_endian env =
    { env with endian = Big_endian }

  let as_little_endian env =
    { env with endian = Little_endian }

  let bit_length env =
    8 * (env.vlen - 1) + env.end_bit - env.start_bit + 1

  let byte_length env =
    env.vlen

  let is_valid env =
    (env.blen <= String.length env.buf)
    && (env.vstart >= 0)
    && (env.blen >= env.vstart + env.vlen)
    && (env.start_bit >= 0) && (env.start_bit < 8)
    && (env.end_bit >= 0) && (env.end_bit < 8)

  let is_non_empty env =
    (is_valid env) && (env.vlen > 0) && (bit_length env > 0)

  let is_byte_aligned env =
    (is_valid env) && (env.start_bit = 0) && (env.end_bit = 7)

  let byte_at env offset =
    if offset < 0 then
      raise (Invalid_access "Env.byte_at");
    if not (is_byte_aligned env) then
      raise (Invalid_op "Env.byte_at");
    if offset >= env.vlen then
      raise (Insufficient_data "Env.byte_at");
    String.get env.buf (env.vstart + offset)

  let set_byte_at env b offset =
    if offset < 0 then
      raise (Invalid_access "Env.set_byte_at");
    if offset >= env.vlen then
      raise (Insufficient_data "Env.set_byte_at");
    String.set env.buf (env.vstart + offset) b

  let vector_at env offset len =
    if offset < 0 || len < 0 then
      raise (Invalid_access "Env.vector_at");
    if not (is_byte_aligned env) then
      raise (Invalid_op "Env.vector_at");
    if offset + len > env.vlen then
      raise (Insufficient_data "Env.vector_at");
    { env with
        vstart = env.vstart + offset;
        vlen = len }

  let set_vector_at env offset src =
    let src_len = String.length src in
      if offset < 0 then
	raise (Invalid_access "Env.set_vector_at");
      if not (is_non_empty env) || not (is_byte_aligned env) then
	raise (Invalid_op "Env.set_vector_at");
      if offset + src_len > env.vlen then
	raise (Insufficient_data "Env.set_vector_at");
      String.blit src 0 env.buf (env.vstart + offset) src_len

  let byte_blit src_env dest_env =
    if not (is_byte_aligned src_env) || not (is_byte_aligned dest_env)
      || (endian src_env) <> (endian dest_env)
    then raise (Invalid_op "Env.byte_blit");
    if byte_length dest_env < byte_length src_env then
      raise (Insufficient_data "Env.byte_blit");
    String.blit src_env.buf src_env.vstart dest_env.buf dest_env.vstart src_env.vlen

  let skip_bytes env len =
    if len < 0 then
      raise (Invalid_op "Env.skip_bytes");
    if len > env.vlen then begin
      raise (Insufficient_data "Env.skip_bytes");
    end;
    { env with
        vstart = env.vstart + len;
        vlen = env.vlen - len }

  let bit_at env bit_offset =
    if bit_offset < 0 then
      raise (Invalid_access "Env.bit_at");
    if bit_offset >= bit_length env then
      raise (Insufficient_data "Env.bit_at");
    let bit_offset = env.start_bit + bit_offset in
    let skipped_bytes = bit_offset / 8 in
    let bit_offset = bit_offset mod 8 in
    let target_byte =
      int_of_char (String.get env.buf (env.vstart + skipped_bytes))
    in (target_byte lsr bit_offset) land 0x1

  let set_bit_at env bit_offset bit =
    if bit_offset < 0 then
      raise (Invalid_access "Env.set_bit_at");
    if bit <> 0 && bit <> 1 then
      raise (Invalid_op "Env.set_bit_at");
    if bit_offset >= bit_length env then
      raise (Insufficient_data "Env.set_bit_at");
    let bit_offset = env.start_bit + bit_offset in
    let skipped_bytes = bit_offset / 8 in
    let skipped_bits = bit_offset mod 8 in
    let byte_offset = env.vstart + skipped_bytes in
    let byte = int_of_char (String.get env.buf byte_offset) in
    let new_byte = byte lor ((bit land 0x1) lsl skipped_bits) in
      String.set env.buf byte_offset (char_of_int new_byte)

  let bit_vector_at env bit_offset bit_len =
    if bit_offset < 0 || bit_len < 0 then
      raise (Invalid_access "Env.bit_vector_at");
    if bit_offset + bit_len >= bit_length env then
      raise (Insufficient_data "Env.bit_vector_at");
    let offs_bits = env.start_bit + bit_offset in
    let offs_bytes =  offs_bits / 8 in
    let offs_bits = offs_bits mod 8 in
    let len_bits = offs_bits + bit_len in
    let len_bytes = len_bits / 8 in
    let len_bits = len_bits mod 8 in
      { env with
          vstart = env.vstart + offs_bytes;
          start_bit = offs_bits;
          vlen = len_bytes + 1;
          end_bit = len_bits }

  (* Byte-wise bitblit.  A good candidate for rewrite in C, and for
     unrolling word-blits.  The source and dest buffers could be
     identical and the views could overlap; this is not handled.  *)
  let bit_op src_env dest_env bitop =
    if not (is_valid src_env) || not (is_valid dest_env) then
      raise (Invalid_op "Env.bit_op");
    if bit_length src_env > bit_length dest_env then
      raise (Insufficient_data "Env.bit_op");
    let total_bits = bit_length src_env in
    let bits_done = ref 0 in
    let s_offs = ref src_env.vstart in
    let s_offs_limit = src_env.vstart + src_env.vlen - 1 in
    let s_bit_start = ref src_env.start_bit in
    let s_bit_end = ref (if src_env.vlen = 1 then src_env.end_bit else 7) in
    let d_offs = ref dest_env.vstart in
    let d_offs_limit = dest_env.vstart + dest_env.vlen - 1 in
    let d_bit_start = ref dest_env.start_bit in
    let d_bit_end = ref (if dest_env.vlen = 1 then dest_env.end_bit else 7) in
      while !bits_done < total_bits do
        let nbits = 1 + min (!s_bit_end - !s_bit_start) (!d_bit_end - !d_bit_start) in
        let s = int_of_char (String.get src_env.buf !s_offs) in
        let d = int_of_char (String.get dest_env.buf !d_offs) in
        let mask = ((0x1 lsl nbits) - 1) in
        let sbits = (s lsr !s_bit_start) land mask in
        let sbits = sbits lsl !d_bit_start in
        let mask = mask lsl !d_bit_start in
        let dbits = d land mask in
        let mask = lnot mask in
        let d = (d land mask) lor (bitop sbits dbits) in
          String.set dest_env.buf !d_offs (char_of_int d);
          bits_done := !bits_done + nbits;
          s_bit_start := !s_bit_start + nbits;
          if !s_bit_start = 8 then begin
            incr s_offs;
            s_bit_start := 0;
          end;
          d_bit_start := !d_bit_start + nbits;
          if !d_bit_start = 8 then begin
            incr d_offs;
            d_bit_start := 0
          end;
          s_bit_end := if !s_offs = s_offs_limit then src_env.end_bit else 7;
          d_bit_end := if !d_offs = d_offs_limit then dest_env.end_bit else 7
      done

  let bit_blit src_env dest_env =
    bit_op src_env dest_env (fun s d -> s)

  let skip_bits env len =
    if len < 0 then
      raise (Invalid_op "Env.skip_bits");
    if len > bit_length env then
      raise (Insufficient_data "Env.skip_bits");
    let skipped_bits = len + env.start_bit in
    let skipped_bytes = skipped_bits / 8 in
    let skipped_bits = skipped_bits mod 8 in
      { env with
          vstart = env.vstart + skipped_bytes;
          vlen = env.vlen - skipped_bytes;
          start_bit = skipped_bits }

  let num_set_bits env =
    if not (is_valid env) then
      raise (Invalid_op "Env.num_set_bits");
    let rec nset_bits b st ed ac =
      if st > ed then ac
      else nset_bits b (st + 1) ed (ac + ((b lsr st) land 0x1))
    in
    let rec iter_bytes offs ac =
      if offs >= env.vlen then ac
      else begin
        let st = (if offs = 0 then env.start_bit else 0) in
        let ed = (if offs = env.vlen - 1 then env.end_bit else 7) in
        let b = int_of_char (String.get env.buf (env.vstart + offs)) in
          iter_bytes (offs + 1) (ac + (nset_bits b st ed 0))
      end
    in
      iter_bytes 0 0

  let align_start env bit_boundary =
    if bit_boundary < 0 || bit_boundary mod 8 <> 0 || not (is_valid env) then
      raise (Invalid_op "Env.align_start");
    let byte_boundary = bit_boundary / 8 in
    let byte_aligned_env =
      if env.start_bit <> 0 then
        { env with
            start_bit = 0;
            vstart = env.vstart + 1;
            vlen = env.vlen - 1 }
      else
        env
    in
    let shift = byte_aligned_env.vstart mod byte_boundary in
    let shift = if shift = 0 then 0 else byte_boundary - shift in
    let aligned_env =
      { byte_aligned_env with
          vstart = byte_aligned_env.vstart + shift;
          vlen = byte_aligned_env.vlen - shift }
    in
      if not (is_valid aligned_env) then
        raise (Invalid_op "Env.align_start");
      aligned_env

  let align_end env bit_boundary =
    if bit_boundary < 0 || bit_boundary mod 8 <> 0 || not (is_valid env) then
      raise (Invalid_op "Env.align_end");
    let byte_boundary = bit_boundary / 8 in
    let shift = (env.vstart + env.vlen) mod byte_boundary in
    let shift = if shift = 0 then 0 else byte_boundary - shift in
    let aligned_env =
      { env with
          vlen = env.vlen + shift;
          end_bit = 7 }
    in
      if not (is_valid aligned_env) then
        raise (Invalid_op "Env.align_start");
      aligned_env

  let sub env offset len =
    if offset < 0 || len < 0 then
      raise (Invalid_access "Env.sub");
    if offset + len > env.vlen then
      raise (Insufficient_data "Env.sub");
    { env with
        vstart = env.vstart + offset;
        vlen = len }

  let bit_sub env offset len =
    if offset < 0 || len < 0 then
      raise (Invalid_access "Env.bit_sub");
    if offset + len > bit_length env then
      raise (Insufficient_data "Env.bit_sub");
    let skipped_bits = env.start_bit + offset in
    let start_skipped_bytes = skipped_bits / 8 in
    let new_start_bit = skipped_bits mod 8 in
    let skipped_bits = new_start_bit + len - 1 in
    let len_skipped_bytes = skipped_bits / 8 in
    let new_end_bit = skipped_bits mod 8 in
      { env with
          vstart = env.vstart + start_skipped_bytes;
          start_bit = new_start_bit;
          vlen = len_skipped_bytes + 1;
          end_bit = new_end_bit }

  let read_raw env offset len =
    if offset < 0 || len < 0 then
      raise (Invalid_access "Env.read_raw");
    if offset + len > env.vlen then
      raise (Insufficient_data "Env.read_raw");
    String.sub env.buf (env.vstart + offset) len

  let write_raw env offset src src_offset len =
    if offset < 0 || len < 0 then
      raise (Invalid_access "Env.write_raw");
    if not (is_valid env) then
      raise (Invalid_op "Env.write_raw");
    if offset + len > env.vlen || src_offset + len > String.length src then
      raise (Insufficient_data "Env.write_raw");
    String.blit src src_offset env.buf (env.vstart + offset) len

  let marshal_bytes env (data : byte_data) =
    match data with
      | `None ->
          (sub env 0 0), env
      | `Str s ->
          let slen = String.length s in
            write_raw env 0 s 0 slen;
            (sub env 0 slen), (skip_bytes env slen)
      | `Frag t ->
          let tlen = byte_length t in
            byte_blit t env;
            (sub env 0 tlen), (skip_bytes env tlen)
      | `Fill fn ->
          fn env
end

module type FP_bit_elem = sig
  type t
  type v
  val rep_to_env : t -> Env.t
  val env_to_rep : Env.t -> t
  val read : t -> v
  val write : v -> t -> unit
  val unmarshal : Env.t -> t * Env.t
  val marshal : Env.t -> v -> t * Env.t
  val copy : t -> Env.t -> t * Env.t
  val of_int : int -> v
  val to_int : v -> int
end

module FP_bit : (FP_bit_elem with type v = int) = struct
  type t = Env.t
  type v = int

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_valid t) || (Env.bit_length t) <> 1 then
      raise (Env.Invalid_op "FP_bit.env_to_rep");
    (t : t)

  let read (t : t) =
    (Env.bit_at t 0 : v)

  let write (v : v) (t : t) =
    if v <> 0 && v <> 1 then
      raise Arg_out_of_bounds;
    Env.set_bit_at t 0 v

  let unmarshal env =
    ((Env.bit_sub env 0 1) : t), (Env.skip_bits env 1)

  let marshal env (v : v) =
    write v env;
    ((Env.bit_sub env 0 1) : t), (Env.skip_bits env 1)

  let copy (t : t) env =
    marshal env (read t)

  let of_int (i : int) = i

  let to_int (v : v) = v
end

module FP_bit_vector = struct
  type t = Env.t
  type v = int

  let size (t : t) =
    Env.bit_length t

  let unmarshal len env =
    ((Env.bit_vector_at env 0 len) : t), (Env.skip_bits env len)

  let read_elem idx (t : t) =
    Env.bit_at t idx

  let write_elem idx (v : v) (t : t) =
    Env.set_bit_at t idx v

  let marshal env (t : t) =
    let len = Env.bit_length t in
    Env.bit_blit t env;
    ((Env.bit_vector_at env 0 len) : t), (Env.skip_bits env len)

  let copy (t : t) env =
    marshal env t
end

module type FP_elem = sig
  include FP_bit_elem

  val size : int
end

module FP_byte : (FP_elem with type v = char) = struct
  type t = Env.t
  type v = char

  let size = 1

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_byte_aligned t) || (Env.byte_length t <> size)
    then raise (Env.Invalid_op "FP_byte.env_to_rep");
    (t : t)

  let read (t : t) =
    (Env.byte_at t 0 : v)

  let write (v : v) (t : t) =
    Env.set_byte_at (rep_to_env t) v 0

  let unmarshal env =
    (Env.sub env 0 1 : t), (Env.skip_bytes env 1)

  let marshal env (v : v) =
    write v env;
    unmarshal env

  let copy (t : t) env =
    marshal env (read t)

  let of_int (i : int) = char_of_int i

  let to_int (v : v) = int_of_char v
end

module FP_int16 : (FP_elem with type v = int) = struct
  type t = Env.t
  type v = int

  let size = 2

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_byte_aligned t) || (Env.byte_length t <> size)
    then raise (Env.Invalid_op "FP_int16.env_to_rep");
    (t : t)

  let read (t : t) =
    let b0, b1 = int_of_char (Env.byte_at t 0), int_of_char (Env.byte_at t 1) in
    let i, sign = match Env.endian t with
      | Env.Little_endian -> b0 + (b1 lsl 8), b1 lsr 7
      | Env.Big_endian -> (b0 lsl 8) + b1, b0 lsr 7
    in
      ((if sign = 0 then i else (i land 0x7fff) - 0x8000) : v)

  let write (v : v) (t : t) =
    if v < -0x8000 || v > 0x7fff then
      raise Arg_out_of_bounds;
    let env = rep_to_env t in
    let sign = v < 0 in
    let b0 = char_of_int (v land 0xff) in
    let b1 = char_of_int ((if sign then 0x8000 else 0x0) + ((v lsr 8) land 0x7f)) in
      match Env.endian env with
        | Env.Big_endian ->
            Env.set_byte_at env b1 0;
            Env.set_byte_at env b0 1
        | Env.Little_endian ->
            Env.set_byte_at env b0 0;
            Env.set_byte_at env b1 1

  let unmarshal env =
    (Env.sub env 0 2 : t), (Env.skip_bytes env 2)

  let marshal env (v : v) =
    write v env;
    unmarshal env

  let copy (t : t) env =
    marshal env (read t)

  let of_int (i : int) = i

  let to_int (v : v) = v
end

module FP_uint16 : (FP_elem with type v = int) = struct
  type t = Env.t
  type v = int

  let size = 2

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_byte_aligned t) || (Env.byte_length t <> size)
    then raise (Env.Invalid_op "FP_uint16.env_to_rep");
    (t : t)

  let read (t : t) =
    let b0, b1 = int_of_char (Env.byte_at t 0), int_of_char (Env.byte_at t 1) in
    let i = match Env.endian t with
      | Env.Little_endian -> b0 + (b1 lsl 8)
      | Env.Big_endian -> (b0 lsl 8) + b1
    in
      (i : v)

  let write (v : v) (t : t) =
    if (v lsr 16) > 0 then
      raise Arg_out_of_bounds;
    let env = rep_to_env t in
    let b0 = char_of_int (v land 255) in
    let b1 = char_of_int ((v lsr 8) land 255) in
      match Env.endian env with
        | Env.Big_endian ->
            Env.set_byte_at env b1 0;
            Env.set_byte_at env b0 1
        | Env.Little_endian ->
            Env.set_byte_at env b0 0;
            Env.set_byte_at env b1 1

  let unmarshal env =
    (Env.sub env 0 2 : t), (Env.skip_bytes env 2)

  let marshal env (v : v) =
    write v env;
    unmarshal env

  let copy (t : t) env =
    marshal env (read t)

  let of_int (i : int) = i

  let to_int (v : v) = v
end

module FP_int32 : (FP_elem with type v = Int32.t) = struct
  type t = Env.t
  type v = Int32.t

  let size = 2 * FP_int16.size

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_byte_aligned t) || (Env.byte_length t <> size)
    then raise (Env.Invalid_op "FP_int32.env_to_rep");
    (t : t)

  let read (t : t) =
    let module I = Int32 in
    let t0, e0 = FP_int16.unmarshal t in
    let t1, _ = FP_int16.unmarshal e0 in
    let i0 = I.of_int (FP_int16.read t0) in
    let i1 = I.of_int (FP_int16.read t1) in
    let i = match Env.endian t with
      | Env.Big_endian -> I.add (I.shift_left i0 16) i1
      | Env.Little_endian -> I.add i0 (I.shift_left i1 16)
    in
      (i : v)

  let unmarshal env =
    (Env.sub env 0 size : t), (Env.skip_bytes env size)

  let marshal env (v : v) =
    let module I = Int32 in
    let i0 = I.to_int (I.logand v 65535l) in
    let i1 = I.to_int (I.shift_right v 16) in
    let next =
      match Env.endian env with
        | Env.Big_endian ->
            snd (FP_int16.marshal (snd (FP_int16.marshal env i1)) i0)
        | Env.Little_endian ->
            snd (FP_int16.marshal (snd (FP_int16.marshal env i0)) i1)
    in
      (Env.sub env 0 size : t), next

  let write (v : v) (t : t) =
    ignore (marshal (rep_to_env t) v)

  let copy (t : t) env =
    marshal env (read t)

  let of_int (i : int) = Int32.of_int i

  let to_int (v : v) = Int32.to_int v
end

module FP_uint32 : (FP_elem with type v = Int32.t) = struct
  type t = Env.t
  type v = Int32.t

  let size = 2 * FP_int16.size

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_byte_aligned t) || (Env.byte_length t <> size)
    then raise (Env.Invalid_op "FP_int32.env_to_rep");
    (t : t)

  let read (t : t) =
    let module I = Int32 in
    let t0, e0 = FP_int16.unmarshal t in
    let t1, _ = FP_int16.unmarshal e0 in
    let i0 = I.of_int (FP_int16.read t0) in
    let i1 = I.of_int (FP_int16.read t1) in
    let i = match Env.endian t with
      | Env.Big_endian -> I.add (I.shift_left i0 16) i1
      | Env.Little_endian -> I.add i0 (I.shift_left i1 16)
    in
      (i : v)

  let unmarshal env =
    (Env.sub env 0 size : t), (Env.skip_bytes env size)

  let marshal env (v : v) =
    let module I = Int32 in
    let i0 = I.to_int (I.logand v 65535l) in
    let i1 = I.to_int (I.shift_right v 16) in
    let next =
      match Env.endian env with
        | Env.Big_endian ->
            snd (FP_int16.marshal (snd (FP_int16.marshal env i1)) i0)
        | Env.Little_endian ->
            snd (FP_int16.marshal (snd (FP_int16.marshal env i0)) i1)
    in
      (Env.sub env 0 size : t), next

  let write (v : v) (t : t) =
    ignore (marshal (rep_to_env t) v)

  let copy (t : t) env =
    marshal env (read t)

  let of_int (i : int) =
    if i < 0 then  (* TODO: check max limit in case where nativeint = int64 *)
      raise Arg_out_of_bounds;
    Int32.of_int i

  let to_int (v : v) =
    if v < 0l then
      raise Arg_out_of_bounds;
    Int32.to_int v

  let of_int64 (i : Int64.t) : v =
    if i < 0L || i > 0xffffffffL then
      raise Arg_out_of_bounds;
    Int64.to_int32 i

  let to_int64 (v : v) =
    if v < 0l then
      Int64.add (Int64.of_int32 (Int32.logand v 0x7fffffffl)) 0x80000000L
    else
      Int64.of_int32 v
end

module FP_int64 : (FP_elem with type v = Int64.t) = struct
  type t = Env.t
  type v = Int64.t

  let size = 2 * FP_int32.size

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_byte_aligned t) || (Env.byte_length t <> size)
    then raise (Env.Invalid_op "FP_int64.env_to_rep");
    (t : t)

  let read (t : t) =
    let module I = Int64 in
    let t0, e0 = FP_int32.unmarshal t in
    let t1, _ = FP_int32.unmarshal e0 in
    let i0 = I.of_int32 (FP_int32.read t0) in
    let i1 = I.of_int32 (FP_int32.read t1) in
    let i = match Env.endian t with
      | Env.Big_endian -> I.add (I.shift_left i0 32) i1
      | Env.Little_endian -> I.add i0 (I.shift_left i1 32)
    in
      (i : v)

  let unmarshal env =
    (Env.sub env 0 size : t), (Env.skip_bytes env size)

  let marshal env (v : v) =
    let module I = Int64 in
    let i0 = I.to_int32 (I.logand v 4294967295L) in
    let i1 = I.to_int32 (I.shift_right v 32) in
    let next =
      match Env.endian env with
        | Env.Big_endian -> begin
            snd (FP_int32.marshal (snd (FP_int32.marshal env i1)) i0)
          end
        | Env.Little_endian ->
            snd (FP_int32.marshal (snd (FP_int32.marshal env i0)) i1)
    in
      (Env.sub env 0 size : t), next

  let write (v : v) (t : t) =
    ignore (marshal (rep_to_env t) v)

  let copy (t : t) env =
    marshal env (read t)

  let of_int (i : int) = Int64.of_int i

  let to_int (v : v) = Int64.to_int v
end

module type FP_array_sig = sig
  type t
  type v
  type elem_v
  val rep_to_env : t -> Env.t
  val env_to_rep : Env.t -> t
  val size : t -> int
  val unmarshal : int -> Env.t -> t * Env.t
  val read_elem : int -> t -> elem_v
  val read : t -> v
  val read_raw : t -> string
  val write_elem : int -> elem_v -> t -> unit
  val write : v -> t -> unit
  val write_raw : string -> t -> unit
  val marshal : Env.t -> v -> t * Env.t
  val copy : t -> Env.t -> t * Env.t
end

module FP_array (E : FP_elem)
  : (FP_array_sig with type v = E.v array and type elem_v = E.v) =
struct
  type t = Env.t
  type v = E.v array
  type elem_v = E.v

  let rep_to_env (t : t) = (t : Env.t)

  let env_to_rep (t : Env.t) =
    if not (Env.is_byte_aligned t) || (Env.byte_length t) mod E.size <> 0
    then raise (Env.Invalid_op "FP_array.env_to_rep");
    (t : t)

  let size (t : t) =
    Env.byte_length t

  let read_elem idx (t : t) =
    let elem = Env.sub t (idx * E.size) E.size in
      E.read (E.env_to_rep elem)

  let write_elem idx (e : elem_v) (t : t) =
    let elem = Env.sub t (idx * E.size) E.size in
    E.write e (E.env_to_rep elem)

  let read (t : t) =
    Array.init ((Env.byte_length t) / E.size) (fun idx -> read_elem idx t)

  let read_raw (t : t) =
    Env.read_raw t 0 (size t)

  let unmarshal len env =
    let alen = len * E.size in
      (Env.sub env 0 alen : t), (Env.skip_bytes env alen)

  let marshal env (a : v) =
    let alen = (Array.length a) * E.size in
    let next = Array.fold_left (fun e ae -> snd (E.marshal e ae)) env a in
      (Env.sub env 0 alen : t), next

  let write (a : v) (t : t) =
    ignore (marshal t a)

  let write_raw src (t : t) =
    Env.write_raw t 0 src 0 (String.length src)

  let copy (t : t) env =
    marshal env (read t)
end

module FP_byte_vector  = FP_array (FP_byte)
module FP_int16_vector = FP_array (FP_int16)
module FP_int32_vector = FP_array (FP_int32)
module FP_int64_vector = FP_array (FP_int64)
