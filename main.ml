module SCT = Size_change_termination

let _ =
  SCT.setOption "show_lambda" true; (* to show the x0 parameter, used for lambda counting in PML *)
  SCT.size_bound:=1;
  SCT.depth_bound:=2;
  SCT.setOption "show_all_compositions" false;
  SCT.setOption "initial_collapse_of_graph" false;
  SCT.setOption "use_calling_context" true;

  SCT.setOption "show_initial_call_graph" false ;
  SCT.setOption "show_final_call_graph" false ;
  SCT.setOption "show_all_steps" false ;
  SCT.setOption "show_coherents" false ;
  SCT.setOption "show_decreasing_terms" false ;
  SCT.setOption "show_nondecreasing_coherents" false ;
  SCT.setOption "show_summary_TC" false;
  SCT.setOption "use_approximates" true;
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
                        (SCT.add_call_set (List.sort compare (fst call) , List.sort compare (snd call)) (try SCT.Call_Graph.find (g,f) !graph
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


let set_verbosity v =
  print_string "verbosity level of "; print_int v; print_newline();
  if v>0 then (show_clauses := true; SCT.setOption "show_summary_TC" true);
  if v>1 then SCT.setOption "show_initial_call_graph" true;
  if v>2 then SCT.setOption "show_final_call_graph" true;
  if v>3 then SCT.setOption "show_all_steps" true;
  if v>4 then (SCT.setOption "show_coherents" true;
               SCT.setOption "show_decreasing_terms" true;
               SCT.setOption "show_nondecreasing_coherents" true);
  if v>5 then SCT.setOption "show_all_compositions" true

let _ =
  let args = [
    ("-B", Arg.Int (fun b -> SCT.size_bound:=b), "<n> set bound 'B' to n (n>0)");
    ("-D", Arg.Int (fun d -> SCT.depth_bound:=d), "<n> set bound 'D' (n>=0)");
    ("-v", Arg.Int (fun v -> set_verbosity v), "<n> shows various information (0<n<7)");
    ("-no_calling_context", Arg.Unit (fun _ -> SCT.setOption "use_calling_context" false), "doesn't use calling contexts to detect more impossible cases");
    ("-no_subsumption", Arg.Unit (fun _ -> SCT.setOption "use_approximates" false), "doesn't use subsumption to simplify the graph of paths");
    ("-collapse_analysis", Arg.Unit (fun _ -> SCT.setOption "initial_collapse_of_graph" true), "collapse the static analysis before building graph of path");
    ("-original_SCT", Arg.Unit (fun _ -> SCT.size_bound:=1; SCT.depth_bound:=0; SCT.setOption "initial_collapse_of_graph" true), "makes the criterion behave like the original SCT");
  ] in
  let help = "usage: sct [-B <n>] [-D <n>] [-v <n>] [...]\n" in
  let help = help^"reads definition from standard input\n" in
  let help = help^"available options:" in
  Arg.parse args print_endline help;
  Printexc.print main ()
