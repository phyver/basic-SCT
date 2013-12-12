open Size_change_termination

(* types *)

(* terms appearing on the right of a definition
 * the variables appearing in the term can either be parameters appearing on
 * the left of the "=" symbol, or function names, or external functions *)
type term = Constr of string*term | Tuple of term list | Var of string | App of term*term

(* one clause of a definition is given by
 *   - a function name
 *   - a calling context
 *   - a term
 *)
type clause = string * term list * term


(* related printing functions *)
let rec print_term t = match t with
  | Var(x) -> print_string x
  | App(t1, t2) -> print_term t1;
                   print_string " ";
                   print_string "("; print_term t2; print_string ")"
  | Tuple(l) -> print_string "(";
                Size_change_termination.print_list ", " print_term l;
                print_string ")"
  | Constr(ct, Tuple([])) -> print_string (ct^"[]")
  | Constr(ct, Tuple(l)) -> print_string (ct^"[");
                            Size_change_termination.print_list ", " print_term l;
                            print_string "]"
  | Constr(ct, t) -> print_string (ct^"[");
                     print_term t;
                     print_string "]"

let rec print_calling_context c = Size_change_termination.print_list " " print_term c

let print_clause c =
  let f, c, t = c in
  print_string (f^" : ");
  print_calling_context c;
  print_string " -> ";
  print_term t;
  print_newline()

(*
let print_pml_def (l:clause list) : unit =
  let rec args n = if n = 0 then [] else ("x"^(string_of_int n))::args (n-1) in

  let rec aux f a l =
    match l with
      [] -> ()
    | (f', c, t)::l ->
        assert (f=f');
        assert (a = List.length c);
        print_string "    | ";
        print_calling_context c;
        print_string " -> ";
        print_term t;
        print_newline();
        aux f a l
  in
  match l with
    [] -> assert false
  | (f,[],t)::[] -> print_string (y^" = "); print_term t
  | (f,[],_)::_ -> assert false
  | (f,c,_)::_ ->
      print_string (f^"  %s ");
      let a = List.length c in
      let xs = List.rev (args a) in
      Size_change_termination.print_list " " print_string xs;
      print_string " = match ";
      Size_change_termination.print_list "," print_string xs;
      print_string " with\n";
      aux f a l
*)

(* apply a function to all elements of a list, where the function can use the
 * index number of the element *)
let mapi (f:int -> 'a -> 'b) (l:'a list) : 'b list =
  let rec aux i l = match l with
    | [] -> []
    | x::l -> (f i x)::aux (i+1) l
  in
  aux 0 l

(* transforms the parameters into a calling context for the SCT *)
let get_parameters all : (string * Size_change_termination.term) list=
  (* c: current context
   * b: branch already computed
   * i: number of the function parameter *)
  let rec aux i b c= match c with
    | Var(x) -> [(x, Epsilon(b, i))]
    | Tuple(l) -> List.concat (mapi (fun k t -> aux i (Project (string_of_int k)::b) t) l)
    | Constr(ct, t) -> aux i ((RemoveVariant ("#"^ct))::b) t
    | App(_,_) -> assert false
  in
  List.concat (mapi (fun i c -> aux i [] c) all)



let process_args args environment : Size_change_termination.call =
  let rec aux (a:term) = match a with
    | Var(x) ->
        if List.mem_assoc x environment
        then List.assoc x environment
        else Sum([(Infty,[],77)])
    | Tuple(l) ->  Record(mapi (fun i a -> (string_of_int i, aux a)) l)
    | Constr(c, a) -> Variant("#"^c, aux a)
    | App(_,_) -> Sum([(Infty,[],66)])
  in
  mapi (fun i a -> (i, aux a)) args

let process_clause c list_functions = match c with f, c, t ->
  let environment = get_parameters c in
  let rec aux (args:term list) (t:term) : (string*Size_change_termination.call) list =
    match t with
    | Var(x) ->
        if List.mem_assoc x environment
        then []
        else if List.mem x list_functions
        then [ (x, process_args args environment) ]
        else failwith ("'"^x^"' is nowhere to be found...")
    | Tuple(l) ->  List.concat (List.map (aux []) l)
    | Constr(_, t) -> aux [] t
    | App(t1, t2) -> (aux (t2::args) t1) @ (aux [] t2)
  in
  aux [] t
