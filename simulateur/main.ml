open Scheduler
open Filename
open Netlist
open Netlist_exec
open Unix

let name = Sys.argv.(1)
let cname = basename (if check_suffix name ".net" then chop_suffix name ".net" else name)

let p = Netlist.read_file name
let () =
try
Netlist_exec.convert cname
        (Scheduler.schedule p)
        (if (Array.length Sys.argv >= 3) then int_of_string Sys.argv.(2) else 42)
        (if (Array.length Sys.argv >= 4) then Sys.argv.(3) else "");

      Printf.printf "Compilation du programme.. %!";
      let res = Unix.system ("g++ -std=c++0x -o simulator_"^cname^" simulator_"^cname^".cpp") in
      match res with
        | WEXITED i when i = 0 ->
          Printf.printf "OK!\nExécution de la netlist.. \n%!";
          Unix.execv ("./simulator_"^cname) [||];
          exit 0
        | _ -> Printf.printf "Problème à la compilation."; exit 1
with
  | Combinational_cycle -> (print_string "Erreur! Boucle combinatoire: la netlist n'est pas valide."; exit 1);
