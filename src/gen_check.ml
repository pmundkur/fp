open Types

type nod =
  | Nvar of Ident.t
  | Nshadow of Ident.t

let ident_of = function
  | Nvar i -> i
  | Nshadow i -> i

let nod_compare n1 n2 =
  Ident.compare (ident_of n1) (ident_of n2)

module Dependencies = Set.Make (struct type t = nod let compare = nod_compare end)
module DepMap = Map.Make (struct type t = nod let compare = nod_compare end)


(* A generic folder over the fields with base type in a struct.  The
   callback func gets called with the current accumulator, field id,
   type, and attributes.
*)
let rec base_node_fold fmt func arg init_acc =
  let rec bn_fold acc fmt =
    let fields = fst fmt in
      List.fold_left
        (fun a f ->
           match f with
             | Tfield_name (i, ft, fal) ->
                 (match ft with
                    | Ttype_base _ ->
                        func arg a i ft fal
                    | Ttype_struct s ->
                        bn_fold a s
                    | Ttype_map (e, m) ->
                        StringMap.fold
                          (fun cn (i, ce, s) a -> bn_fold a s)
                          m a
                    | Ttype_array (e, s) ->
                        bn_fold a s
                    | Ttype_label ->
                        a)
             | Tfield_align _ ->
                 a)
        acc fields
  in
    bn_fold init_acc fmt

(* To create the dependency graph, we need to first compute the nodes
   of the graph for each format.  Nodes are only created for fields
   that can have value expressions (i.e., fields with base types).
*)
let nodes_of fmt =
  base_node_fold fmt
    (fun arg acc i ft fal ->
       match ft with
         | Ttype_base (Tbase_primitive _) ->
             (Nvar i) :: acc
         | Ttype_base (Tbase_vector _) ->
             (Nvar i) :: (Nshadow i) :: acc
         | _ ->
             assert false)
    () []

let init_graph fmt =
  let nods = nodes_of fmt in
    List.fold_left
      (fun m n -> DepMap.add n Dependencies.empty m)
      DepMap.empty nods

let rec dependencies = function
  | Texp_unit ->  [], []
  | Texp_var i ->  [ i ], []
  | Texp_const_bit _
  | Texp_const_byte _
  | Texp_const_int16 _
  | Texp_const_uint16 _
  | Texp_const_int32 _
  | Texp_const_uint32 _
  | Texp_const_int64 _
  | Texp_const_int _ -> [], []
  | Texp_apply (i, args) ->
      let fname = Ident.name_of i in
      let possible_shadow = (fname = "byte_sizeof" || fname = "bit_sizeof") in
      let deps, sdeps =
        List.fold_left
          (fun (d_in, sd_in) e ->
             let d, sd = dependencies e in
               (List.rev_append d d_in), (List.rev_append sd sd_in))
          ([], []) args in
        if possible_shadow then
          (* TODO: lookup type of i. if a vector, then use shadow arg; else use real arg. *)
          [], []
        else
          deps, sdeps

let update_dependencies env dg i ft fal =
  let nd = Nvar i in
  let deps = DepMap.find nd dg in
  let updated_deps =
    List.fold_left
      (fun a fa -> match fa with
         | Tattrib_max _
         | Tattrib_min _
         | Tattrib_const _
         | Tattrib_default _ ->
             a
         | Tattrib_value e ->
             (* TODO *)
             a
         | Tattrib_variant v ->
             a
      )
      deps fal in
    DepMap.add nd updated_deps dg

let populate_graph env dg fmt =
  base_node_fold fmt
    (fun env acc i ft fal ->
       update_dependencies env acc i ft fal)
    env dg
