open Types

(* This section of this file checks that the branches specified in
   value attributes are exhaustive and not redundant.  We do this by
   viewing branch specifications as patterns.

   Each branch of the value attribute is viewed as a struct pattern.
   A struct pattern is a list of branch patterns, as is a pattern
   vector.  A pattern matrix is composed of rows of pattern vectors.

   The algorithms implemented below are taken from "Warnings for
   pattern matching", by Luc Maranget, in "Journal of Functional
   Programming", vol 17, issue 3, May 2007.
*)

let list_take n list =
  if n < 0 then raise (Invalid_argument "take");
  let rec take acc n = function
    | [] ->
        if n > 0 then raise (Failure "take") else List.rev acc, []
    | hd :: tl as l ->
        if n = 0 then List.rev acc, l else take (hd :: acc) (n - 1) tl
  in
    take [] n list

let get_case_branch_info branch_info case_name =
  let cn, _, st =
    try
      StringMap.find case_name branch_info.field_map.map_type_desc
    with
      | Not_found ->
          assert false
  in
    cn, st.classify_fields

let make_default binfo =
  { pattern = Pt_any;
    branch_info = binfo }

let make_default_vector binfos =
  List.map make_default binfos

let make_default_from_branch branch_info case_name =
  make_default_vector (snd (get_case_branch_info branch_info case_name))

let specialize_branch_info branch_info case_name =
  snd (get_case_branch_info branch_info case_name)

let rec specialize_vector branch_info case_name = function
  | [] ->
      Some []
  | { pattern = Pt_constructor (cn, (Pt_struct plist)) } :: ptail ->
      if case_name = Location.node_of cn then
        Some (plist @ ptail)
      else
        None
  | { pattern = Pt_any } :: ptail ->
      Some ((make_default_from_branch branch_info case_name) @ ptail)

and specialize_matrix branch_info case_name matrix =
  List.fold_left
    (fun acc pvec ->
       match (specialize_vector branch_info case_name pvec) with
         | None -> acc
         | Some v -> v :: acc)
    [] (List.rev matrix)

let unspecialize_vector branch_info case_name pvec =
  let cn, cbi = get_case_branch_info branch_info case_name in
  let plist, ptail = list_take (List.length cbi) pvec in
    { pattern = Pt_constructor (cn, (Pt_struct plist));
      branch_info = branch_info } :: ptail

let default_matrix matrix =
  List.fold_left
    (fun acc -> function
       | { pattern = Pt_any } :: ptail -> ptail :: acc
       | _ -> acc)
    [] (List.rev matrix)

module StringSet = Set.Make (struct type t = string let compare = compare end)

let column_signature matrix =
  let do_row set = function
    | { pattern = Pt_constructor (cn, _) } :: ptail ->
        StringSet.add (Location.node_of cn) set
    | _ ->
        set
  in
    List.fold_left do_row StringSet.empty matrix

exception Found of Asttypes.case_name * branch_info list
let get_a_missing_constructor signature branch_info =
  StringSet.iter
    (fun s -> assert (StringMap.mem s branch_info.field_map.map_type_desc))
    signature;
  try
    StringMap.iter
      (fun s (cn, _, st) ->
         if StringSet.mem s signature
         then ()
         else raise (Found (cn, st.classify_fields)))
      branch_info.field_map.map_type_desc;
    None
  with
    | Found (cn, binfos) -> Some (cn, binfos)

let is_complete_signature signature branch_info =
  match get_a_missing_constructor signature branch_info with
    | None -> true
    | Some _ -> false

(* This is the core usefulness routine, and implements the U_rec
   function of the paper.
*)
let rec is_useful_pattern matrix pattern =
  match matrix with
    | [] ->
        (* This case arises when the first pattern with a certain
           constructor is introduced. *)
        true
    | [] :: ps ->
        (* This case arises when specialized matrices are generated
           for leaf branches, i.e. structs that don't have any
           classify fields.  At this stage, the pattern is completely
           matched by a matrix row, and hence is not useful. *)
        List.iter (fun p -> assert (List.length p = 0)) ps;
        assert (List.length pattern = 0);
        false
    | p :: ps ->
        List.iter (fun p -> assert (List.length p = List.length pattern)) matrix;
        match pattern with
          | { pattern = Pt_constructor (cn, (Pt_struct plist));
              branch_info = bi }
            :: ptail ->
              let sp_matrix =
                specialize_matrix bi (Location.node_of cn) matrix
              in
                is_useful_pattern sp_matrix (plist @ ptail)
          | { pattern = Pt_any; branch_info = bi }
            :: ptail ->
              let signature = column_signature matrix in
                if is_complete_signature signature bi then
                  let sp_input =
                    StringSet.fold
                      (fun s acc ->
                         (specialize_matrix bi s matrix,
                          (make_default_from_branch bi s) @ ptail)
                         :: acc)
                      signature []
                  in
                    List.fold_left
                      (fun acc (m, v) -> acc || is_useful_pattern m v)
                      false sp_input
                else
                  is_useful_pattern (default_matrix matrix) ptail
          | [] ->
              assert false


(* This is the core exhaustiveness checking routine, and implements
   the I algorithm of the paper.
*)
exception Found of string * branch list
let rec get_unmatched_pattern matrix binfos =
  match matrix with
    | [] ->
        Some (make_default_vector binfos)
    | [] :: ps ->
        List.iter (fun p -> assert (List.length p = 0)) ps;
        assert (List.length binfos = 0);
        None
    | p :: ps ->
        List.iter (fun p' -> assert (List.length p' = List.length p)) ps;
        assert (List.length binfos = List.length p);
        let signature = column_signature matrix in
        let bi, tl_bis = List.hd binfos, List.tl binfos in
        let sp_matrix s = specialize_matrix bi s matrix in
        let sp_bi s = (specialize_branch_info bi s) @ tl_bis in
          if is_complete_signature signature bi then
            begin
              try
                StringSet.iter
                  (fun s ->
                     match get_unmatched_pattern (sp_matrix s) (sp_bi s) with
                       | Some p ->  raise (Found (s, p))
                       | None -> ())
                  signature;
                None
              with
                | Found (s, p) -> Some (unspecialize_vector bi s p)
            end
          else
            match get_unmatched_pattern (default_matrix matrix) tl_bis with
              | None ->
                  None
              | Some p ->
                  let pattern =
                    match get_a_missing_constructor signature bi with
                      | None ->
                          Pt_any
                      | Some (cn, bis) ->
                          Pt_constructor (cn, Pt_struct (make_default_vector bis))
                  in
                    Some ({ pattern = pattern; branch_info = bi } :: p)


(* This is the driver for the pattern usefulness checker.  It takes a
   field_value list as input, and checks whether the specified pattern
   matching has any redundancies and is complete.
*)

exception Redundant_branch_pattern of Location.t
exception Unmatched_branch_pattern of Ident.t * struct_pattern

let raise_redundant_branch_pattern loc =
  raise (Redundant_branch_pattern loc)
let raise_unmatched_branch_pattern id stp =
  raise (Unmatched_branch_pattern (id, stp))

let check_field_value_list fid fvl st =
  let final_matrix =
    List.fold_left
      (fun m fv ->
         (* Note that the pattern computed for the default field_value
            is an empty pattern when the pattern matrix m is empty.
            This will trigger a bunch of asserts if the default is
            followed by any subsequent field values.  But the
            typechecking phase ensures that the default is the last in
            the sequence. *)
         let pattern =
           match fv.field_value_desc with
             | Tvalue_auto
             | Tvalue_default _ ->
                 make_default_vector st.classify_fields
             | Tvalue_branch { struct_pattern = Pt_struct pattern } ->
                 pattern
         in
           if is_useful_pattern m pattern then
             List.rev (pattern :: (List.rev m))
           else
             raise_redundant_branch_pattern fv.field_value_loc)
      [] fvl
  in
    match get_unmatched_pattern final_matrix st.classify_fields with
      | None ->
          ()
      | Some p ->
          raise_unmatched_branch_pattern fid (Pt_struct p)

let rec check_struct st =
  let rec do_field st fe =
    match fe.field_entry_desc with
      | Tfield_name (fid, ft, fal) ->
          (match ft with
             | Ttype_base _ ->
                 do_attribs fid fal st
             | Ttype_struct st ->
                 check_struct st
             | Ttype_map (_, mt) ->
                 StringMap.iter
                   (fun _ (_, _, st) -> check_struct st)
                   mt.map_type_desc
             | Ttype_array (_, st) ->
                 check_struct st
             | Ttype_label ->
                 ())
      | Tfield_align _ ->
          ()
  and do_attribs fid fal st =
    List.iter
      (fun fa ->
         match fa.field_attrib_desc with
           | Tattrib_max _ | Tattrib_min _
           | Tattrib_const _ | Tattrib_default _ | Tattrib_variant _ ->
               ()
           | Tattrib_value fvl ->
               check_field_value_list fid fvl st)
      fal
  in
    List.iter (do_field st) st.entries

let handle_pattern_exception e =
  (match e with
     | Redundant_branch_pattern loc ->
         Printf.fprintf stderr "%s: redundant branch guard"
           (Location.pr_location loc)
     | Unmatched_branch_pattern (id, sp) ->
         Printf.fprintf stderr "%s: non-exhaustive value guards for field %s (example: %s)"
           (Location.pr_location (Ident.location_of id)) (Ident.pr_ident_name id) (pr_struct_pattern sp)
     | e ->
         raise e
  );
  Printf.fprintf stderr "\n";
  exit 1

let check_formats fmts =
  try
    Ident.iter (fun _ st -> check_struct st) fmts
  with
    | e -> handle_pattern_exception e
