module SCT = Size_change_termination

(*** utils *)
(* apply a function to all elements of a list, where the function can use the
 * index number of the element *)
let mapi (f:int -> 'a -> 'b) (l:'a list) : 'b list =
  let rec mapi_aux i l = match l with
    | [] -> []
    | x::l -> (f i x)::mapi_aux (i+1) l
  in
  mapi_aux 0 l

(* terms appearing in the definitions *)
type term = Var of string | Constr of string*term | Tuple of term list | App of term*term

(* one clause of a definition is given by
 *   - a function name
 *   - a calling context: a list of parameters represented by terms without App
 *   - a term for the body of the definition
 *)
type clause = string * term list * term

(* printing functions *)
let rec print_term paren_app paren_tup t = match t with
  | Var(x) -> print_string x
  | Constr(ct, Tuple([])) -> print_string (ct^"[]")
  | Constr(ct, Tuple(l)) -> print_string (ct^"[");
                            SCT.print_list ", " (print_term false false) l;
                            print_string "]"
  | Constr(ct, t) -> print_string (ct^"[");
                     print_term false false t;
                     print_string "]"
  | Tuple(l) -> if paren_tup then print_string "(";
                SCT.print_list ", " (print_term false false) l;
                if paren_tup then print_string ")"
  | App(t1, t2) -> if paren_app then print_string "(";
                   print_term false false t1;
                   print_string " ";
                   print_term true true t2;
                   if paren_app then print_string ")"

let rec print_calling_context c = SCT.print_list " " (print_term false true) c

let print_clause c = let f, c, t = c in
                     print_string (f^" ");
                     print_calling_context c;
                     print_string " = ";
                     print_term false true t;
                     print_newline()

(* transforms the patterns of the clause into an environment for the SCT: the
 * pattern Cons[x,xs] will give x:= pi_1 Cons- x0 and xs = pi_2 Cons- x0 *)
let patterns2environment all : (string * SCT.term) list=
  (* c: current context
   * b: branch already computed
   * i: number of the function parameter *)
  let rec patterns2environment_aux i b c= match c with
    | Var(x) -> [(x, SCT.Epsilon(b, i))]
    | Tuple(l) -> List.concat (mapi (fun k t -> patterns2environment_aux i (SCT.Project (string_of_int k)::b) t) l)
    | Constr(ct, t) -> patterns2environment_aux i ((SCT.RemoveVariant ("#"^ct))::b) t
    | App(_,_) -> assert false
  in
  List.concat (mapi (fun i c -> patterns2environment_aux i [] c) all)


let infty arity =
  let rec infty_aux arity acc =
    if arity = 0
    then acc
    else infty_aux (arity-1) ((SCT.Infty,[],arity-1)::acc)
  in SCT.Sum(infty_aux arity [])


let process_args args environment arity: SCT.call =
  let rec process_args_aux (a:term) = match a with
    | Var(x) ->
        if List.mem_assoc x environment
        then List.assoc x environment
        else infty arity
    | Tuple([]) -> infty arity
    | Tuple(l) ->  SCT.Record(mapi (fun i a -> (string_of_int i, process_args_aux a)) l)
    | Constr(c, a) -> SCT.Variant("#"^c, process_args_aux a)
    | App(_,_) -> infty arity
  in
  mapi (fun i a -> (i, process_args_aux a)) args


let process_clause c list_functions = match c with f, c, t ->
  let environment = patterns2environment c in
  let rec process_clause_aux (args:term list) (t:term) : (string*SCT.call) list =
    match t with
    | Var(x) ->
        if List.mem_assoc x environment
        then []
        else if List.mem x list_functions
        then [ (x, process_args args environment (List.length c)) ]
        else failwith ("'"^x^"' is nowhere to be found...")
    | Tuple(l) ->  List.concat (List.map (process_clause_aux []) l)
    | Constr(_, t) -> process_clause_aux [] t
    | App(t1, t2) -> (process_clause_aux (t2::args) t1) @ (process_clause_aux [] t2)
  in
  process_clause_aux [] t
