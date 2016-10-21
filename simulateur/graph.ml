exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

exception Cycle
let has_cycle g =
  let rec explore = function
    | [] -> ()
    | noeud::q when noeud.n_mark = InProgress -> raise Cycle
    | noeud::q -> noeud.n_mark <- InProgress; explore noeud.n_link_to;
      noeud.n_mark <- Visited; explore q
  in
  try
    clear_marks g;
    explore g.g_nodes; false
  with
  | Cycle -> true

let topological g =
  let lst = ref([]) in
  let rec explore = function
    | [] -> ()
    | n::q when n.n_mark = NotVisited ->
       n.n_mark <- InProgress;
       explore n.n_link_to;
       n.n_mark <- Visited;
       lst := n.n_label::(!lst);
       explore q
    | _::q -> explore q
  in
  clear_marks g;
  explore (find_roots g);  !lst
