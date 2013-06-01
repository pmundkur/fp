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

open Types
open Format

module Typeinfo = struct
  let lib_module_name = function
    | Tbase_primitive Tprim_bit         -> "FP_bit"
    | Tbase_primitive Tprim_byte        -> "FP_byte"
    | Tbase_primitive Tprim_int16       -> "FP_int16"
    | Tbase_primitive Tprim_uint16      -> "FP_uint16"
    | Tbase_primitive Tprim_int32       -> "FP_int32"
    | Tbase_primitive Tprim_uint32      -> "FP_uint32"
    | Tbase_primitive Tprim_int64       -> "FP_int64"

    | Tbase_vector (Tprim_bit, _)       -> "FP_bit_vector"
    | Tbase_vector (Tprim_byte, _)      -> "FP_byte_vector"
    | Tbase_vector (Tprim_int16, _)     -> "FP_int16_vector"
    | Tbase_vector (Tprim_uint16, _)    -> "FP_uint16_vector"
    | Tbase_vector (Tprim_int32, _)     -> "FP_int32_vector"
    | Tbase_vector (Tprim_uint32, _)    -> "FP_uint32_vector"
    | Tbase_vector (Tprim_int64, _)     -> "FP_int64_vector"

  let value_type_name bt =
    (lib_module_name bt) ^ ".v"

  let literal_of_const = function
    | Texp_const_bit i
    | Texp_const_int16 i
    | Texp_const_uint16 i
    | Texp_const_int i   -> string_of_int i

    | Texp_const_byte i  -> Printf.sprintf "'%c'" (char_of_int i)

    | Texp_const_int32 i -> (Int32.to_string i) ^ "l"

    | Texp_const_uint32 i
    | Texp_const_int64 i -> (Int64.to_string i) ^ "L"

    | _ -> assert false

  let class_arg_type fid ft =
    match ft with
      | Ttype_base bt ->
          (lib_module_name bt) ^ ".t"
      | Ttype_struct _ ->
          String.capitalize (Ident.name_of fid) ^ ".o"
      | Ttype_array _  ->
          String.capitalize (Ident.name_of fid) ^ ".o array"
      | Ttype_map _ ->
          String.capitalize (Ident.name_of fid) ^ ".t"
      | Ttype_label ->
          ""
      | Ttype_format fname ->
          String.capitalize (Location.node_of fname) ^ ".o"
end

module Variant = struct
  let type_name id =
    (Ident.name_of id) ^ "_t"

  let to_string_name id =
    (Ident.name_of id) ^ "_to_string"

  let to_name id =
    (Ident.name_of id) ^ "_to_value"

  let of_name id =
    (Ident.name_of id) ^ "_of_value"

  let base_type_of ft =
    match ft with
      | Ttype_base bt -> bt
      | _ -> assert false

  let gen_type ff id ft v =
    fprintf ff "@[<v 2>type %s = [@," (type_name id);
    List.iter (fun (_, cn, _) ->
                 fprintf ff "| `%s@," (String.capitalize (Location.node_of cn))
              ) v.variant_desc;
    fprintf ff "| `Unknown of %s" (Typeinfo.value_type_name (base_type_of ft));
    fprintf ff "@]@,]@,"

  let gen_of ff id ft v =
    fprintf ff "@[<v 2>let %s v : %s =@," (of_name id) (type_name id);
    fprintf ff "@[<v 2>match v with@,";
    List.iter (fun (e, cn, _) ->
                 fprintf ff "| %s -> `%s@,"
                   (Typeinfo.literal_of_const e.exp_desc)
                   (String.capitalize (Location.node_of cn))
              ) v.variant_desc;
    fprintf ff "| v -> `Unknown v";
    fprintf ff "@]@]@,"

  let gen_to ff id ft v =
    fprintf ff "@[<v 2>let %s (t : %s) =@," (to_name id) (type_name id);
    fprintf ff "@[<v 2>match t with@,";
    List.iter (fun (e, cn, _) ->
                 fprintf ff "| `%s -> %s@,"
                   (String.capitalize (Location.node_of cn))
                   (Typeinfo.literal_of_const e.exp_desc)
              ) v.variant_desc;
    fprintf ff "| `Unknown v -> v";
    fprintf ff "@]@]@,"

  let gen_to_string ff id ft v =
    fprintf ff "@[<v 2>let %s (t : %s) =@," (to_string_name id) (type_name id);
    fprintf ff "@[<v 2>match t with@,";
    List.iter (fun (e, cn, _) ->
                 fprintf ff "| `%s -> \"%s\"@,"
                   (String.capitalize (Location.node_of cn))
                   (String.capitalize (Location.node_of cn))
              ) v.variant_desc;
    fprintf ff "| `Unknown v -> %s.to_string v"
      (Typeinfo.lib_module_name (base_type_of ft));
    fprintf ff "@]@]@,"

  let generate_defs ff id ft v =
    gen_type ff id ft v;        fprintf ff "@,";
    gen_of ff id ft v;          fprintf ff "@,";
    gen_to ff id ft v;          fprintf ff "@,";
    gen_to_string ff id ft v;   fprintf ff "@,"

end

module Object = struct
  let arg_name id = Ident.name_of id ^ "_arg"

  let args_of st =
    Ident.fold (fun id (ft, _) o_args ->
                  let at = Typeinfo.class_arg_type id ft in
                    if at <> "" then (id, at) :: o_args else o_args
               ) st.fields []

  let field_method ff id ft fa =
    let getter = "get_" ^ Ident.name_of id in
    let c_arg = arg_name id in
      match ft, fa.field_attrib_variant with
        | Ttype_base ((Tbase_primitive _) as bt), None ->
            fprintf ff "@,method %s = %s.read %s"
              getter (Typeinfo.lib_module_name bt) c_arg
        | Ttype_base ((Tbase_vector _) as bt), None ->
            fprintf ff "@,method %s_at indx = %s.read_elem indx %s"
              getter (Typeinfo.lib_module_name bt) c_arg
        | Ttype_base bt, Some _ ->
            fprintf ff "@,method %s = %s (%s.read %s)"
              getter (Variant.of_name id) (Typeinfo.lib_module_name bt) c_arg
        | Ttype_struct _, _ | Ttype_format _, _ | Ttype_map _, _->
            fprintf ff "@,method %s = %s" getter c_arg
        | Ttype_array _, _ ->
            fprintf ff "@,method %s_at indx = %s.(indx)" getter c_arg
        | Ttype_label, _ ->
            ()

  let generate ff st =
    let args = List.map (fun (id, at) ->
                           Printf.sprintf "(%s : %s)" (arg_name id) at
                        ) (args_of st)
    in
      fprintf ff "@[<v 2>class o %s = object" (String.concat " " args);
      Ident.iter (fun id (ft, fa) -> field_method ff id ft fa) st.fields;
      fprintf ff "@]@,end@,"
end

module Struct = struct
  let module_name ?(suffix="") st_id =
    let suffix = if suffix = "" then "" else "_" ^ (String.lowercase suffix) in
      String.capitalize (Ident.name_of st_id) ^ suffix

  let start_module ff ?(suffix="") st_id =
    fprintf ff "@[<v 2>module %s = struct@," (module_name st_id ~suffix)

  let end_module ff =
    fprintf ff "@]@,end@,"

  let generate_variants ff (st, deps) =
    Ident.iter (fun id (ft, fattribs) ->
                  match fattribs.field_attrib_variant with
                    | None -> ()
                    | Some (v, _) -> Variant.generate_defs ff id ft v
               ) st.fields

  let rec generate_map ff typed_env id (bid, mt, deps) =
    start_module ff id;
    StringMap.iter (fun cnm (_, _, st) ->
                      generate ff typed_env id ~suffix:cnm (st, deps)
                   ) mt.map_type_desc;
    fprintf ff "@[<v 2>type t =@,";
    StringMap.iter (fun cnm _ ->
                      fprintf ff "| %s of %s.o@,"
                        (String.capitalize cnm) (module_name id ~suffix:cnm)
                   ) mt.map_type_desc;
    fprintf ff "@]@,";
    end_module ff

  and generate ff typed_env st_id ?(suffix="") (st, deps) =
    start_module ff ~suffix st_id;
    generate_variants ff (st, deps);

    (* nested structs *)
    Ident.iter (fun id (ft, fattribs) ->
                  match ft with
                    | Ttype_struct st
                    | Ttype_array (_, st) ->
                        generate ff typed_env id (st, deps)
                    | Ttype_map (bid, mt) ->
                        generate_map ff typed_env id (bid, mt, deps)
                    | _ ->
                        ()
               ) st.fields;

    Object.generate ff st;

    (* Opaque formats will be referred to by their repn type t, so we
       need this type defined for all structs, not just for maps.
    *)
    fprintf ff "type t = Env.t@,";

    (* TODO: struct unmarshalling/marshalling. This requires computing
       the free variables of the struct, which need to be passed in as
       arguments.  Also, some free variables are special:
       e.g. remaining(), offset(field), etc. in array/vector
       lengths. *)
    end_module ff
end

let open_output fn =
  (* Need to remove read-only files since Open_trunc needs write-permission. *)
  (try Sys.remove fn with _ -> ());
  let op_flags = [ Open_wronly ; Open_creat; Open_trunc; Open_text ] in
  let oc = open_out_gen op_flags 0o444 fn in
  let ff = formatter_of_out_channel oc in
    oc, ff

let close_output (oc, ff) =
  fprintf ff "@?";
  close_out oc

let generate_header ff =
  let argv = Array.to_list Sys.argv in
  let argv = (Filename.basename (List.hd argv)) :: (List.tl argv) in
  let invok = String.concat " " argv in
    fprintf ff "(* This file has been auto-generated using \"%s\". *)@\n@\n" invok

let generate_opens ff =
  fprintf ff "open Fp_lib@\n@\n"

let generate typed_env struct_env ofn =
  let oc, ff = open_output ofn in
    generate_header ff;
    generate_opens ff;
    fprintf ff "@[<v 0>";
    Ident.iter (fun i (fmt, deps) ->
                  Struct.generate ff typed_env i (fmt, deps);
                  fprintf ff "@,"
               ) struct_env;
    fprintf ff "@]";
    close_output (oc, ff)
