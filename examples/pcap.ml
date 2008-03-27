module Pcap_header = struct
  open Sp_lib
  exception Bad_struct of string

  class o
    ~(self_env : Env.t)
    ~(version_major_t : SP_int16.t)
    ~(version_minor_t : SP_int16.t)
    ~(timezone_offset_t : SP_int32.t)
    ~(timestamp_accuracy_t : SP_int32.t)
    ~(snap_len_t : SP_int32.t)
    ~(link_type_t : SP_int32.t) =
  object (self)
    method env =
      self_env
    method sizeof =
      Env.byte_length self_env

    method version_major =
      SP_int16.read version_major_t
    method set_version_major v =
      SP_int16.write v version_major_t

    method version_minor =
      SP_int16.read version_minor_t
    method set_version_minor v =
      SP_int16.write v version_minor_t

    method timezone_offset =
      SP_int32.read timezone_offset_t
    method set_timezone_offset v =
      SP_int32.write v timezone_offset_t

    method timestamp_accuracy =
      SP_int32.read timestamp_accuracy_t
    method set_timestamp_accuracy v =
      SP_int32.write v timestamp_accuracy_t

    method snap_len =
      SP_int32.read snap_len_t
    method set_snap_len v =
      SP_int32.write v snap_len_t

    method link_type =
      SP_int32.read link_type_t
    method set_link_type v =
      SP_int32.write v link_type_t
  end

  let t
      ~version_major
      ~version_minor
      ~timezone_offset
      ~timestamp_accuracy
      ~snap_len
      ~link_type
      env =
    let version_major_t, cursor = SP_int16.marshal env version_major in
    let version_minor_t, cursor = SP_int16.marshal cursor version_minor in
    let timezone_offset_t, cursor = SP_int32.marshal cursor timezone_offset in
    let timestamp_accuracy_t, cursor = SP_int32.marshal cursor timestamp_accuracy in
    let snap_len_t, cursor = SP_int32.marshal cursor snap_len in
    let link_type_t, cursor = SP_int32.marshal cursor link_type in

    let self_env_size = (SP_int16.size + SP_int16.size + SP_int32.size + SP_int32.size + SP_int32.size + SP_int32.size) in
    let self_env = Env.sub env 0 self_env_size in
      (new o
        ~self_env:self_env
        ~version_major_t:version_major_t
        ~version_minor_t:version_minor_t
        ~timezone_offset_t:timezone_offset_t
        ~timestamp_accuracy_t:timestamp_accuracy_t
        ~snap_len_t:snap_len_t
        ~link_type_t:link_type_t), cursor

  let unmarshal env =
    let version_major_t, cursor = SP_int16.unmarshal env in
    let version_minor_t, cursor = SP_int16.unmarshal cursor in
    let timezone_offset_t, cursor = SP_int32.unmarshal cursor in
    let timestamp_accuracy_t, cursor = SP_int32.unmarshal cursor in
    let snap_len_t, cursor = SP_int32.unmarshal cursor in
    let link_type_t, cursor = SP_int32.unmarshal cursor in

    let self_env_size = (SP_int16.size + SP_int16.size + SP_int32.size + SP_int32.size + SP_int32.size + SP_int32.size) in
    let self_env = Env.sub env 0 self_env_size in
      (new o
        ~self_env:self_env
        ~version_major_t:version_major_t
        ~version_minor_t:version_minor_t
        ~timezone_offset_t:timezone_offset_t
        ~timestamp_accuracy_t:timestamp_accuracy_t
        ~snap_len_t:snap_len_t
        ~link_type_t:link_type_t), cursor

  let pretty_print (o : o) =
    let p = print_endline in
      p "[ Pcap.pcap_header ]";
      p (" version_major = " ^ (Printf.sprintf "%u (0x%x)" o#version_major o#version_major));
      p (" version_minor = " ^ (Printf.sprintf "%u (0x%x)" o#version_minor o#version_minor));
      p (" timezone_offset = " ^ (Printf.sprintf "%lu (0x%lx)" o#timezone_offset o#timezone_offset));
      p (" timestamp_accuracy = " ^ (Printf.sprintf "%lu (0x%lx)" o#timestamp_accuracy o#timestamp_accuracy));
      p (" snap_len = " ^ (Printf.sprintf "%lu (0x%lx)" o#snap_len o#snap_len));
      p (" link_type = " ^ (Printf.sprintf "%lu (0x%lx)" o#link_type o#link_type));
      ()
end

module Pcap_entry = struct
  open Sp_lib
  exception Bad_struct of string

  class o
    ~(self_env : Env.t)
    ~(sec_t : SP_int32.t)
    ~(usec_t : SP_int32.t)
    ~(real_len_t : SP_int32.t)
    ~(data_t : SP_byte_vector.t) =
  object (self)
    method env =
      self_env
    method sizeof =
      Env.byte_length self_env

    method sec =
      SP_int32.read sec_t
    method set_sec v =
      SP_int32.write v sec_t

    method usec =
      SP_int32.read usec_t
    method set_usec v =
      SP_int32.write v usec_t

    method real_len =
      SP_int32.read real_len_t
    method set_real_len v =
      SP_int32.write v real_len_t

    method data_array =
      SP_byte_vector.read data_t
    method data_env =
      SP_byte_vector.rep_to_env data_t
    method data_raw =
      SP_byte_vector.read_raw data_t
    method data_length =
      SP_byte_vector.size data_t
  end

  let t
      ~sec
      ~usec
      ~real_len
      ~(data : Env.byte_data)
      env =
    let sec_t, cursor = SP_int32.marshal env sec in
    let usec_t, cursor = SP_int32.marshal cursor usec in
    let cap_len_t, cursor = (SP_int32.env_to_rep (Env.sub cursor 0 SP_int32.size),
                             Env.skip_bytes cursor SP_int32.size) in
    let real_len_t, cursor = SP_int32.marshal cursor real_len in
    let data_t, cursor = Env.marshal_bytes cursor data in
    let data_t = SP_byte_vector.env_to_rep data_t in
    let data_t_size = SP_byte_vector.size data_t in

    let self_env_size = (SP_int32.size + SP_int32.size + SP_int32.size + SP_int32.size + data_t_size) in
    let self_env = Env.sub env 0 self_env_size in
      SP_int32.write (SP_int32.of_int data_t_size) cap_len_t;
      (new o
        ~self_env:self_env
        ~sec_t:sec_t
        ~usec_t:usec_t
        ~real_len_t:real_len_t
        ~data_t:data_t), cursor

  let unmarshal env =
    let sec_t, cursor = SP_int32.unmarshal env in
    let usec_t, cursor = SP_int32.unmarshal cursor in
    let cap_len_t, cursor = SP_int32.unmarshal cursor in
    let real_len_t, cursor = SP_int32.unmarshal cursor in
    let data_t, cursor = SP_byte_vector.unmarshal (Int32.to_int (SP_int32.read cap_len_t)) cursor in

    let self_env_size = (SP_int32.size + SP_int32.size + SP_int32.size + SP_int32.size + SP_byte_vector.size data_t) in
    let self_env = Env.sub env 0 self_env_size in
      (new o
        ~self_env:self_env
        ~sec_t:sec_t
        ~usec_t:usec_t
        ~real_len_t:real_len_t
        ~data_t:data_t), cursor

  let pretty_print (o : o) =
    let p = print_endline in
      p "[ Pcap.pcap_entry ]";
      p (" sec = " ^ (Printf.sprintf "%lu (0x%lx)" o#sec o#sec));
      p (" usec = " ^ (Printf.sprintf "%lu (0x%lx)" o#usec o#usec));
      p (" real_len = " ^ (Printf.sprintf "%lu (0x%lx)" o#real_len o#real_len));
      ()
end
