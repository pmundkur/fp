(* The algorithms implemented below are adapted from "Warnings for
   pattern matching", by Luc Maranget, in "Journal of Functional
   Programming", vol 17, issue 3, May 2007.

   A struct pattern is a list of branch patterns, as is a pattern
   vector.  A pattern matrix is composed of rows of pattern vectors.
*)

open Types

let make_default_vector branch_info case_name =
  let (_, _, st) =
    try
      StringMap.find case_name branch_info.field_map
    with
      | Not_found ->
          assert false in
  let maker (cid, mt) =
    { pattern = Pt_any;
      branch_info = { field = cid;
                      field_map = mt } }
  in
    List.map maker st.classify_fields

let rec specialize_vector branch_info case_name = function
  | [] ->
      Some []
  | { pattern = Pt_constructor (cn, (Pt_struct plist)) } :: ptail ->
      if case_name = Location.node_of cn then
        Some (plist @ ptail)
      else
        None
  | { pattern = Pt_any } :: ptail ->
      Some ((make_default_vector branch_info case_name) @ ptail)

and specialize_matrix branch_info case_name matrix =
  List.fold_left
    (fun acc pvec ->
       match (specialize_vector branch_info case_name pvec) with
         | None -> acc
         | Some v -> v :: acc)
    [] (List.rev matrix)

let default_matrix matrix =
  List.fold_left
    (fun acc -> function
       | { pattern = Pt_any } :: ptail -> ptail :: acc
       | _ -> acc)
    [] (List.rev matrix)

module StringSet = Set.Make (struct type t = string let compare = compare end)

(* This gets the constructor signature of the first column of the
   matrix. *)
let column_signature matrix =
  let do_row set = function
    | { pattern = Pt_constructor (cn, _) } :: ptail ->
        StringSet.add (Location.node_of cn) set
    | _ ->
        set
  in
    List.fold_left do_row StringSet.empty matrix

let is_complete_signature signature branch_info =
  StringSet.iter
    (fun s -> assert (StringMap.mem s branch_info.field_map))
    signature;
  StringMap.fold
    (fun s _ acc -> acc && StringSet.mem s signature)
    branch_info.field_map true

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
        List.iter
          (fun p -> assert (List.length p = List.length pattern))
          matrix;
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
                          (make_default_vector bi s) @ ptail)
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




(* This is the driver for the pattern usefulness checker.  It takes a
   field_value list as input, and checks whether the specified pattern
   matching has any redundancies and/or is complete.
*)

let make_default_from_matrix m =
  let rec from_vector = function
    | [] ->
        []
    | { branch_info = bi } :: ptail ->
        { pattern = Pt_any; branch_info = bi } :: (from_vector ptail)
  in
    match m with
      | [] -> []
      | p :: ps -> from_vector p

exception Redundant_branch_pattern of Location.t
let raise_redundant_branch_pattern loc =
  raise (Redundant_branch_pattern loc)

let check_field_value_list fvl =
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
             | Tvalue_default _ ->
                 make_default_from_matrix m
             | Tvalue_branch { struct_pattern = Pt_struct pattern } ->
                 pattern
         in
           if is_useful_pattern m pattern then
             List.rev (pattern :: (List.rev m))
           else
             raise_redundant_branch_pattern fv.field_value_loc)
      [] fvl
  in
    (* TODO: check final_matrix for redundancies *)
    final_matrix
