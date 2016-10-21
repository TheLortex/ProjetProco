open Netlist_ast
open Graph
open Netlist
open Format
open Scheduler

let soi = string_of_int

let ramsize = ref(0)
let ramop = ref([])

module Smap = Map.Make(String)
type vardata = {pos: int; ind: int; siz : int}
type vars = {mutable vars : vardata Smap.t} (*position puis *)

(*indice puis taille*)
let i = ref(0) and j = ref(0) and k = ref(0)
let varinfo = {vars = Smap.empty}
let i' = ref(0) and j' = ref(0) and k' = ref(0)
let reginfo = {vars = Smap.empty}

let to_number = function
  | VBit b -> if b then 1 else 0
  | VBitArray barray -> Array.fold_right (fun b x -> 2*x + (if b then
      1 else 0)) barray 0

let print_arg = function
  | Avar ident ->
     let t = Smap.find ident varinfo.vars in
     "(select(b_"^(soi t.pos)^","^(soi (t.siz))^","^(soi t.ind)^"))"
  | Aconst(x) -> (soi (to_number x))^"ULL"

let get_size = function
  | Avar ident -> (Smap.find ident varinfo.vars).siz
  | Aconst(VBit _) -> 1
  | Aconst(VBitArray n) -> Array.length n

let translate_expr = function
  | Earg(arg) -> "("^(print_arg arg)^")"
  | Ereg(ident) -> let t = Smap.find ident reginfo.vars in
     "(select(r_"^(soi t.pos)^","^(soi (t.siz))^","^(soi t.ind)^"))"
  | Enot(arg) -> "((~"^(print_arg arg)^")&((1ULL << "^(soi (get_size
							      arg))^")-1))"
  | Ebinop(Or,a1,a2) -> "("^(print_arg a1)^"|"^(print_arg a2)^")"
  | Ebinop(Xor,a1,a2) -> "("^(print_arg a1)^"^"^(print_arg a2)^")"
  | Ebinop(And,a1,a2) -> "("^(print_arg a1)^"&"^(print_arg a2)^")"
  | Ebinop(Nand,a1,a2) -> "(!("^(print_arg a1)^"&"^(print_arg a2)^"))"
  | Emux(sel,a1,a2) -> "(ok("^(print_arg sel)^") ? "^(print_arg a1)^" : "^(print_arg a2)^"})"
  | Econcat(a1,a2) ->
     let s2 = get_size a2 in
     ("((1 << "^(soi s2)^")*("^(print_arg a1)^") + ("^(print_arg a2)^"))")
  | Eslice(i1,i2,arg) -> (* a ameliorer *)
     "(select("^(print_arg arg)^","^(soi (i2-i1+1))^","^(soi i1)^"))"
  | Eselect(i, arg) -> "(select("^(print_arg arg)^",1,"^(soi i)^"))"
  | Eram(addr_size, word_size, read_addr, write_enable, write_addr,
	 data) -> "ramread(ram,"^(soi word_size)^","^(print_arg read_addr)^")"
  | _ -> "" (* Reste Erom *)

let addvar reg x q =
  let t = match q with
    | TBit -> 1
    | TBitArray n -> n
  in
  if reg then begin
    if !j' + !k' + t < 64 then
      (j' := !j' + !k'; k' := t)
    else
      (i' := !i' + 1; j' := 0; k' := t);
    print_int (!i');
    print_int (!j');
    reginfo.vars <- Smap.add x {pos = !i'; ind = !j'; siz = t} reginfo.vars

  end
  else begin
    if !j + !k + t < 64 then
      (j := !j + !k; k := t)
    else
      (i := !i + 1; j := 0; k := t);
    varinfo.vars <- Smap.add x {pos = !i; ind = !j; siz = t} varinfo.vars
  end

let check_reg f = function
  | (_,Ereg(i)) -> addvar true i (TBitArray (get_size (Avar i)))
  | (_,Eram(i,w,ra,we,wa,d)) -> ramop := Eram(i,w,ra,we,wa,d)::!ramop;
    ramsize := max (!ramsize) ((2 lsl i)*w); ()
  | _ -> ()

let print_line f (i,expr) =
  let d = Smap.find i varinfo.vars in
  output_string f ("b_"^(soi d.pos)^" &= maskbit("^(soi d.ind)^","^(soi
  d.siz)^");
b_"^(soi d.pos)^" |= ("^(translate_expr expr)^" << "^(soi d.ind)^");
  //"^(i)^" \n\n")


let update_reg f x dreg =
  output_string f ("r_"^(soi dreg.pos)^" &= maskbit("^(soi dreg.ind)^","^(soi
  dreg.siz)^");\n
r_"^(soi dreg.pos)^" |= ("^(translate_expr (Earg (Avar x)))^" << "^(soi dreg.ind)^");\n")


let update_ram f (Eram(r,w,_,we,wa,d)) = output_string f
  ("ramwrite(ram,"^(soi w)^","^(print_arg we)^","^(print_arg
  wa)^","^(print_arg d)^"); \n")


let convert p =
  let f = open_out "sortie.cpp" in
  (* Sauvegarde des tailles des variables et écriture de l'entête du fichier *)
  Env.iter (addvar false) p.p_vars;
  output_string f " #include <iostream> \n #include \"bitop.h\" \n
   \n using namespace std; \n";
  (* On écrit les registres en variables globales et on regardes les
  instructions de ram *)
  List.iter (check_reg f) p.p_eqs;
  print_int (!i');
  for p=0 to !i' do
    output_string f ("ull r_"^(soi p)^"(0);\n")
  done;
  output_string f ("ull* ram = new ull["^(soi (!ramsize/8+1))^"]; \n");

  (* Écriture de la fonction cycle et de ses paramètres*)
  output_string f "void cycle(int no";
  List.iter (fun x -> (output_string f (", unsigned long long int
  "^x))) p.p_inputs;
  output_string f ") {\n";


  for p=0 to !i do
    output_string f ("unsigned long long int b_"^(soi p)^"(0);\n")
  done;

  List.iter (fun x -> let d = Smap.find x varinfo.vars in
    (output_string f ("b_"^(soi d.pos)^" &= maskbit("^(soi
  d.ind)^","^(soi d.siz)^");\n
b_"^(soi d.pos)^" |= ("^x^" << "^(soi d.ind)^");\n"))) p.p_inputs;
  (* Simulation de la netlist ligne par ligne *)
  List.iter (print_line f) p.p_eqs;
  (* Sauvegarde des registres *)
  Smap.iter (update_reg f) reginfo.vars;
  List.iter (update_ram f) !ramop;
  (* Affichage des sorties *)
  List.iter (fun x -> (output_string f ("cout << \""^x^":\";
    binoutput<"^(soi (get_size(Avar x)))^">("^(print_arg (Avar x))^"); \n"))) p.p_outputs;

  output_string f "\n return;\n}\n
int main() {\n
while(1) {\n";

  (* Dans une boucle infinie on exécute les cycles en demandant à *)
  (* chaque fois l'entrée *)
  List.iter (fun x -> output_string f ("unsigned long long int _"^x^";\n")) p.p_inputs;
  List.iter (fun x -> output_string f ("cout << \""^x^" ? \"; _"^x^"=bininput<"^(soi(get_size(Avar x)))^">();")) p.p_inputs;
  output_string f "cycle(0";
  List.iter (fun x -> output_string f (",_"^x)) p.p_inputs;
  output_string f ");}}";

  flush f;
  close_out f

;;

let p = Netlist.read_file Sys.argv.(1) in
convert(Scheduler.schedule p)
