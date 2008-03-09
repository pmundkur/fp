exception Arg_out_of_bounds

module Env = struct

  exception Invalid_access
  exception Invalid_op

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
    && (env.vstart >= 0) && (env.vlen > 0)
    && (env.blen >= env.vstart + env.vlen)
    && (bit_length env > 0)
    && (env.start_bit >= 0) && (env.start_bit < 8)
    && (env.end_bit >= 0) && (env.end_bit < 8)

  let is_byte_aligned env =
    (env.start_bit = 0) && (env.end_bit = 0)

  let byte_at env offset =
    if offset < 0 || offset >= env.vlen || not (is_byte_aligned env) then
      raise Invalid_access;
    String.get env.buf (env.vstart + offset)

  let set_byte_at env b offset =
    if offset < 0 || offset >= env.vlen then
      raise Invalid_access;
    String.set env.buf (env.vstart + offset) b

  let vector_at env offset len =
    if offset < 0 || len < 0 || offset + len > env.vlen
      || not (is_byte_aligned env)
    then raise Invalid_access;
    { env with
        vstart = env.vstart + offset;
        vlen = len }

  let set_vector_at env offset src =
    let src_len = String.length src in
      if not (is_valid env) || not (is_byte_aligned env)
        || offset < 0 || offset + src_len > env.vlen
      then raise Invalid_op;
      String.blit src 0 env.buf (env.vstart + offset) src_len

  let byte_blit src_env dest_env =
    if not (is_valid src_env) || not (is_valid dest_env)
      || not (is_byte_aligned src_env) || not (is_byte_aligned dest_env)
      || byte_length dest_env < byte_length src_env
    then raise Invalid_op;
    String.blit src_env.buf src_env.vstart dest_env.buf dest_env.vstart src_env.vlen

  let skip_bytes env len =
    if len < 0 || len > env.vlen then
      raise Invalid_op;
    { env with
        vstart = env.vstart + len;
        vlen = env.vlen - len }

  let bit_at env bit_offset =
    if bit_offset < 0 || bit_offset >= bit_length env then
      raise Invalid_op;
    let bit_offset = env.start_bit + bit_offset in
    let skipped_bytes = bit_offset / 8 in
    let bit_offset = bit_offset mod 8 in
    let target_byte =
      int_of_char (String.get env.buf (env.vstart + skipped_bytes))
    in (target_byte lsr bit_offset) land 0x1

  let set_bit_at env bit_offset bit =
    if bit_offset < 0 || bit_offset >= bit_length env
      || (bit <> 0 && bit <> 1)
    then raise Invalid_op;
    let bit_offset = env.start_bit + bit_offset in
    let skipped_bytes = bit_offset / 8 in
    let skipped_bits = bit_offset mod 8 in
    let byte_offset = env.vstart + skipped_bytes in
    let byte = int_of_char (String.get env.buf byte_offset) in
    let new_byte = byte lor ((bit land 0x1) lsl skipped_bits) in
      String.set env.buf byte_offset (char_of_int new_byte)

  let bit_vector_at env bit_offset bit_len =
    if bit_offset < 0 || bit_len < 0 || bit_offset + bit_len >= bit_length env
    then raise Invalid_access;
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
    if not (is_valid src_env) || not (is_valid dest_env)
      || bit_length src_env > bit_length dest_env
    then raise Invalid_op;
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
    if len < 0 || len > bit_length env then
      raise Invalid_op;
    let skipped_bits = len + env.start_bit in
    let skipped_bytes = skipped_bits / 8 in
    let skipped_bits = skipped_bits mod 8 in
      { env with
          vstart = env.vstart + skipped_bytes;
          vlen = env.vlen - skipped_bytes;
          start_bit = skipped_bits }

  let num_set_bits env =
    if not (is_valid env) then
      raise Invalid_op;
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

  let sub env offset len =
    if offset < 0 || len < 0 || offset + len > env.vlen then
      raise Invalid_op;
    { env with
	vstart = env.vstart + offset;
	vlen = len }
end

module SP_byte = struct
  type t = char

  let unmarshal env =
    ((Env.byte_at env 0) : t), (Env.skip_bytes env 1)

  let marshal env (b : t) =
    Env.set_byte_at env b 0;
    Env.skip_bytes env 1
end

module SP_int16 = struct
  type t = int

  let unmarshal env =
    let b0, b1 = int_of_char (Env.byte_at env 0), int_of_char (Env.byte_at env 1) in
    let i = match Env.endian env with
      | Env.Little_endian -> b0 + (b1 lsl 8)
      | Env.Big_endian -> (b0 lsl 8) + b1
    in
      (i : t), (Env.skip_bytes env 2)

  let marshal env (i : t) =
    if (i lsr 16) > 0 then
      raise Arg_out_of_bounds;
    let b0 = char_of_int (i land 255) in
    let b1 = char_of_int ((i lsr 8) land 255) in
      (match Env.endian env with
	| Env.Big_endian ->
	    Env.set_byte_at env b1 0;
	    Env.set_byte_at env b0 1
	| Env.Little_endian ->
	    Env.set_byte_at env b0 0;
	    Env.set_byte_at env b1 1);
      Env.skip_bytes env 2
end

module SP_int32 = struct
  type t = Int32.t

  let unmarshal env =
    let module I = Int32 in
    let i0, e = SP_int16.unmarshal env in
    let i1, _ = SP_int16.unmarshal e in
    let i0 = I.of_int i0 in
    let i1 = I.of_int i1 in
    let i = match Env.endian env with
      | Env.Big_endian -> I.add (I.shift_left i0 16) i1
      | Env.Little_endian -> I.add i0 (I.shift_left i1 16)
    in
      i, (Env.skip_bytes env 4)

  let marshal env i =
    let module I = Int32 in
    let i0 = I.to_int (I.logand i 65535l) in
    let i1 = I.to_int (I.shift_right i 16) in
      match Env.endian env with
	| Env.Big_endian ->
	    SP_int16.marshal (SP_int16.marshal env i1) i0
	| Env.Little_endian ->
	    SP_int16.marshal (SP_int16.marshal env i0) i1
end

module SP_int64 = struct
  type t = Int64.t

  let unmarshal env =
    let module I = Int64 in
    let i0, e = SP_int32.unmarshal env in
    let i1, _ = SP_int32.unmarshal e in
    let i0 = I.of_int32 i0 in
    let i1 = I.of_int32 i1 in
    let i = match Env.endian env with
      | Env.Big_endian -> I.add (I.shift_left i0 32) i1
      | Env.Little_endian -> I.add i0 (I.shift_left i1 32)
    in
      (i : t), (Env.skip_bytes env 8)

  let marshal env (i : t) =
    let module I = Int64 in
    let i0 = I.to_int32 (I.logand i 4294967295L) in
    let i1 = I.to_int32 (I.shift_right i 32) in
      match Env.endian env with
	| Env.Big_endian -> begin
	    SP_int32.marshal (SP_int32.marshal env i1) i0
	  end
	| Env.Little_endian ->
	    SP_int32.marshal (SP_int32.marshal env i0) i1
end

module SP_bit = struct
  type t = int

  let unmarshal env =
    ((Env.bit_at env 0) : t), (Env.skip_bits env 1)

  let marshal env (b : t) =
    if b <> 0 && b <> 1 then
      raise Arg_out_of_bounds;
    Env.set_bit_at env 0 b;
    Env.skip_bits env 1
end

module SP_byte_vector = struct
  type t = Env.t

  let unmarshal len env =
    ((Env.vector_at env 0 len) : t), (Env.skip_bytes env len)

  let marshal env (v : t) =
    Env.byte_blit v env;
    Env.skip_bytes env (Env.byte_length env)
end

module SP_bit_vector = struct
  type t = Env.t

  let unmarshal len env =
    ((Env.bit_vector_at env 0 len) : t), (Env.skip_bits env len)

  let marshal env (v : t) =
    Env.bit_blit v env;
    Env.skip_bits env (Env.bit_length v)
end

