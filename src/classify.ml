open Types

(* check to ensure that a field is not classified multiple times *)

exception Classify_multiple_use of Ident.t

let raise_classify_multiple_use id =
  raise (Classify_multiple_use id)

let rec check_struct st =
  let rec helper st targets =
    Ident.fold
      (fun i ft targs -> check_field i ft targs)
      (snd st)
      targets
  and check_field fid ft targets =
    match ft with
      | Ttype_base _
      | Ttype_label ->
          targets
      | Ttype_struct st ->
          helper st targets
      | Ttype_map (e, mt) ->
          let tid =
            (match e with
               | Texp_var p -> path_tail_ident p
               | _ -> assert false) in
            if List.mem tid targets then
              raise_classify_multiple_use tid;
            StringMap.fold
              (fun _ (_, _, st) targs -> helper st targs)
              mt (tid :: targets)
      | Ttype_array (_, st) ->
          helper st targets
  in
    ignore (helper st [])

let check_multiple_use fmt_env =
  (try
    Ident.iter
      (fun i fmt -> check_struct fmt)
      fmt_env
  with
    | Classify_multiple_use fid ->
        Printf.fprintf stderr "%s: field \"%s\" is used for classification multiple times"
          (Location.pr_location (Ident.location_of fid)) (Ident.name_of fid));
  Printf.fprintf stderr "\n";
  exit 1

