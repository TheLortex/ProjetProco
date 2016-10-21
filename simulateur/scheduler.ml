open Netlist_ast
open Graph

exception Combinational_cycle
exception Pas_trouve


let read_exp exp  =
  let addvar = function
    | Avar(ident) -> [ident]
    | _ -> []
  in
  match exp with
    | Earg(i) -> addvar i
    | Enot(i) -> addvar i
    | Ebinop(_,i,j)  ->
       if i!=j then (addvar i) @ (addvar j) else addvar i
    | Emux(i,j,k) ->
       if i!=j && j!=k && i!=k then
	 (addvar i) @ (addvar j) @ (addvar k)
       else if j=k && i=j then
	 addvar i
       else if j=k then
	 (addvar i) @ (addvar j)
       else
	 (addvar j) @ (addvar k)
    | Econcat(i,j) ->
       if i=j then (addvar i) else (addvar i) @ (addvar j)
    | Eslice(_,_,i) -> (addvar i)
    | Eselect(_,i) -> (addvar i)
    | Erom(_,_,adr) -> (addvar adr)
    | Eram(_,_,adr,_,_,_) -> (addvar adr)
    | Ereg(_) -> []

let schedule p =
  let g = mk_graph () in
  List.iter (add_node g) p.p_inputs;
  List.iter (add_node g) p.p_outputs;
  let rec find id = function
    | [] -> false
    | n::q when n.n_label = id -> true
    | _::q -> find id q
  in
  let add i1 i2 =
    if not(find i1 g.g_nodes) then (add_node g i1);
    if not(find i2 g.g_nodes) then (add_node g i2);
    add_edge g i1 i2
  in
  let rec build_edge = function
    | [] -> ()
    | (ident,exp)::q -> List.iter (add ident) (read_exp exp); build_edge q
  in
  build_edge p.p_eqs; (* Le graphe est construit. *)
  if has_cycle g then
    raise Combinational_cycle
  else begin
    let tri_topol = topological g in
    let rec trouve liste elem = match liste with
      | [] -> raise Pas_trouve
      | (i,e)::q when i = elem -> (i,e)
      | (t,_)::q -> trouve q elem
    in
    let rec buildlist = function
      | [] -> []
      | t::q ->
	try
	  let res = trouve p.p_eqs t in
	   res::(buildlist q)
	 with
	 | Pas_trouve -> buildlist q
    in
    {p_eqs = buildlist (List.rev tri_topol);
     p_inputs = p.p_inputs;
     p_outputs = p.p_outputs;
     p_vars = p.p_vars}
  end
