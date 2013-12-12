open Tools
open Size_change_termination

let _ =
  setOption "show_lambda" true;
  size_bound:=1;
  depth_bound:=2;
  setOption "show_all_steps" false;
  ()


let main () =
    let lexbuf = Lexing.from_channel stdin in

    let clauses = Parser.defs Lexer.token lexbuf in
    let graph = ref Call_Graph.empty in
    let nodes = List.map (function x,_,_ -> x) clauses in

    List.iter (fun clause ->
      print_clause clause; flush_all();
      let g,ct,_ = clause in
      try
        let p=process_clause clause nodes in

        List.iter (function f,call ->
          print_string (g^" -> "^f^"\n");
          print_substitution call;
          print_newline();
          graph := Call_Graph.add (g,f)
                                  (add_call_set (List.sort compare call) (try Call_Graph.find (g,f) !graph
                                                      with Not_found -> Calls_Set.empty))
                                  !graph ;
                  ) p
      with
        Failure(s) ->
          print_string "*** Problem while processing the definitions:\n";
          print_string (">>> "^s^"\n");
          print_string "*** Aborting.\n";
          exit 2
      ) clauses;

      if size_change_termination !graph
      then
        (print_string "OK, all functions terminate\n" ;
        exit 0)
      else
        (print_string "BAD, some functions may not terminate\n" ;
        exit 1)



let _ = Printexc.print main ()
