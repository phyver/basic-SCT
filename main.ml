module SCT = Size_change_termination

let _ =
  SCT.setOption "show_lambda" true; (* to show the x0 parameter, used for lambda counting in PML *)
  SCT.size_bound:=1;
  SCT.depth_bound:=2;
  SCT.setOption "show_initial_call_graph" false ;
  SCT.setOption "show_collapsed_call_graph" false ;
  SCT.setOption "show_final_call_graph" false ;
  SCT.setOption "show_all_steps" false ;
  SCT.setOption "show_coherents" false ;
  SCT.setOption "show_decreasing_terms" false ;
  SCT.setOption "show_nondecreasing_coherents" false ;
  SCT.setOption "show_summary_TC" false;
  ()

let show_clauses = ref false

let main () =
    let lexbuf = Lexing.from_channel stdin in

    let clauses = Parser.defs Lexer.token lexbuf in
    let graph = ref SCT.Call_Graph.empty in
    let nodes = List.map (function x,_,_ -> x) clauses in

    if !show_clauses then
      begin
        print_string "Definitions\n-----------\n";
        List.iter Tools.print_clause clauses;
        print_newline()
      end;
    List.iter (fun clause ->
      let g,ct,_ = clause in
      try
        let p = Tools.process_clause clause nodes in

        List.iter (function f,call ->
          (*
          print_string (g^" -> "^f^"\n");
          SCT.print_substitution call;
          print_newline();
          *)
          graph := SCT.Call_Graph.add
                        (g,f)
                        (SCT.add_call_set (List.sort compare call) (try SCT.Call_Graph.find (g,f) !graph
                                                                    with Not_found -> SCT.Calls_Set.empty))
                        !graph ;
                  ) p
      with
        Failure(s) ->
          print_string "*** Problem while processing the definitions:\n";
          print_string (">>> "^s^"\n");
          print_string "*** Aborting.\n";
          exit 2
      ) clauses;

      if SCT.size_change_termination !graph
      then
        (print_string "OK, all functions terminate\n" ;
        exit 0)
      else
        (print_string "BAD, some functions may not terminate\n" ;
        exit 1)


let _ =
  let args = [
    ("-B", Arg.Int (fun b -> SCT.size_bound:=b), "\t\t\tset bound 'B'");
    ("-D", Arg.Int (fun d -> SCT.depth_bound:=d), "\t\t\tset bound 'D'");
    ("-v", Arg.Unit (fun _ -> SCT.setOption "show_summary_TC" true), "\t\t\tshow the incremental tests");
    ("-show_definitions", Arg.Unit (fun _ -> show_clauses := true), "\tshow the definition (after parsing)");
    ("-show_analysis", Arg.Unit (fun _ -> SCT.setOption "show_initial_call_graph" true), "\tshow the initial control-flow graph given by the static analysis");
    ("-show_graph_of_paths", Arg.Unit (fun _ -> SCT.setOption "show_final_call_graph" true), "\tshow the graph of paths of the control-flow graph");
    ("-show_decreasing", Arg.Unit (fun _ -> SCT.setOption "show_decreasing_terms" true), "\tshow the decreasing arguments of coherent loops");
    ("-show_loops", Arg.Unit (fun _ -> SCT.setOption "show_nondecreasing_coherents" true), "\t\tshow the coherent loops which do not have decreasing arguments\n");
  ] in
  let help = "usage: sct [-B <n>] [-D <n>] [...]\n" in
  let help = help^"reads definition from standard input\n" in
  let help = help^"available options:" in
  Arg.parse args print_endline help;
  Printexc.print main ()
