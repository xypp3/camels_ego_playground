open Ego.Basic

(* create an egraph *)
let graph = EGraph.init ()
(* add expressions *)
let expr =
    let open Sexplib0.Sexp in
        List [Atom "/"; List [Atom "<<"; Atom "a"; Atom "1"]; Atom "2"];;
let _ = EGraph.add_sexp graph expr
(* Convert to graphviz *)

let from = let open Sexplib0.Sexp in 
        Query.of_sexp (List [Atom "<<"; Atom "?a"; Atom "1";]);;
let into = let open Sexplib0.Sexp in 
        Query.of_sexp (List [Atom "*"; Atom "?a"; Atom "2";]);;
let rule = match (Rule.make ~from ~into) with
        | Some x -> x
        | None -> raise Not_found

(*NOTE: To see graph, you need to:
* 1. output string into file (or pipe)
* 2. use system command "dot" as follows `dot -Tsvg > output.svg`
* 3. read svg (I'm using Inkscape's `inkview` command)
*)
let egraph_to_dot_str egraph =
        let g : Odot.graph = EGraph.to_dot egraph in
        let g_str = Odot.string_of_graph g in
        g_str;;

let file_of_string filename dotStr =
        Out_channel.with_open_text filename
                (fun oc -> Out_channel.output_string oc dotStr)


let () = file_of_string "gaffa.dot" (egraph_to_dot_str graph)
let () = print_endline (egraph_to_dot_str graph)
let _ = EGraph.run_until_saturation graph [rule]
(* let () = print_endline (egraph_to_dot_str graph) *)
