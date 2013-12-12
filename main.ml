module SCT = Size_change_termination

let _ =
  SCT.setOption "show_lambda" true;
  SCT.size_bound:=1;
  SCT.depth_bound:=0;
  SCT.setOption "show_all_steps" false;
  ()


let main () =
    let lexbuf = Lexing.from_channel stdin in

    let clauses = Parser.defs Lexer.token lexbuf in
    let graph = ref SCT.Call_Graph.empty in
    let nodes = List.map (function x,_,_ -> x) clauses in

    List.iter (fun clause ->
      Tools.print_clause clause; flush_all();
      let g,ct,_ = clause in
      try
        let p = Tools.process_clause clause nodes in

        List.iter (function f,call ->
          print_string (g^" -> "^f^"\n");
          SCT.print_substitution call;
          print_newline();
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
    ("-B", Arg.Int (fun b -> SCT.size_bound:=b), "set bound 'B'");
    ("-D", Arg.Int (fun d -> SCT.depth_bound:=d), "set bound 'D'");
  ] in
  let help = "usage: sct [-B <n>] [-D <n>]\n" in
  let help = help^"reads definition from standard input\n" in
  let help = help^"available options:" in
  Arg.parse args print_endline help;
  Printexc.print main ()
