(*======================================================================
Copyright Christophe Raffalli & Pierre Hyvernat, Universite de Savoie.

christophe.raffalli@univ-savoie.fr
Pierre.Hyvernat@univ-savoie.fr

This software is a computer program which implements an interpreter
and type-checker for the PML (Proved Meta-Language) computer language.
PML is a language of the ML family with the extra possibility to write
and prove the specifications of your code.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info".

The exercising of this freedom is conditional upon a strong obligation
of giving credits for everybody that distributes a software
incorporating a software ruled by the current license so as all
contributions to be properly identified and acknowledged.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms.
======================================================================*)



(******************************************************************************
 * This is the file implementing the "Size Change Termination Principle",     *
 * adapted from Lee, Jones & Ben-Amram.                                       *
 * The test is described in details in the paper "The Size Change Termination *
 * Principle for Constructor based Languages".                                *
 ******************************************************************************)

(* A couple of differences between this implementation and what is described in
 * the article:
 *
 *   0/ Records rather than tuples
 *   PML uses Records with labels rather than plain tuples. (Tuples are just
 *   records with labels "1", "2", "3" etc.
 *
 *   1/ Default field
 *   PML's records have a "default" field, which is used instead of the whole
 *   record in special cases. One such case is when a record is matched against
 *   a variant destructor. This explains the special case in the definition of
 *   reduction.
 *
 *   2/ Default field, bis
 *   The default field in a record cannot be itself a record. This explains why,
 *   in several places, the Caml variant constructor "Record" is replaced by a
 *   function "_Record" which flattens the default field if necessary.
 *
 *   3/ Partial applications / lambda deficit
 *   This doesn't show in the code but only in the output of the algorithm (in
 *   verbose mode). We consider all the partial applications: a call
 *      f u1 u2 u3
 *   will give rise to 3 calls:
 *      - f u1
 *      - f u1 u2
 *      - f u1 u2 u3
 *   This can be important strange definitions such as
untyped {
val rec f?(? => ? => ?) x  =
  let h = f x in
  fun y ->
    begin
    match y with
       A[y] -> h B[y]
     | _ -> C[]
    end
}
which doesn't terminate whereas
val rec g:(? => ? => ?) x =
  fun y ->
  let h = g x in
    begin
    match y with
       A[y] -> h B[y]
     | _ -> C[]
    end
does terminate

 * 4/ The actual test is incremental: we try with d=0, 1, 2, 4, ...,
 * depth_bound. This seems to be efficient in practice.
 *
 * 5/ The calls also contains a "calling context". This is a list of
 * destructor branches that can be applied to arguments. This allows to keep
 * the information that x starts with an A[] in the following code
 *   val rec f x = match x with A[y] -> f B[x]
 * The call will be represented by
 *   x := B x   |   A- x
 * The right part is the calling context...
 *
 * When composing two calls, we check that those destructors can indeed be
 * applied. In particular, the previous call cannot be composed with itself.
 *)


(*
 * Bureaucratic things
 *)
let debugOptions = ref [
  "additional_sanity_checking" , true                   ;
  "use_approximates" , true                             ;
  "initial_collapse_of_graph", true                     ;
  "use_calling_context", true                           ;

(* verbosity of output messages *)
  "show_lambda" , false                                 ;
  "show_initial_call_graph" , false                     ;
  "show_final_call_graph" , false                       ;
  "show_summary_TC" , false                             ;
  "show_all_steps" , false                              ;
  "show_all_compositions" , false                       ;
  "show_coherents" , false                              ;
  "show_decreasing_terms" , false                       ;
  "show_nondecreasing_coherents" , false                ;
]

let setOption s v =
  let rec aux options s v acc = match options with
      [] -> raise Not_found
    | (s',_)::options when s'=s -> (s',v)::List.rev_append options acc
    | x::options -> aux options s v (x::acc)
  in
  debugOptions := aux !debugOptions s v []

let debugOption s =
  let b = try List.assoc s !debugOptions with Not_found -> assert false
  in b

let ifDebug (s:string) (c:unit->unit) : unit =
  let b = debugOption s in
  if b then c ()


(*************
 * Datatypes *
 *************)

(* Type for integer with a $\infty$ element *)
type z_infty = Number of int | Infty

(*
 * The type of destructors. Projection are along strings because we
 * consider records with labels rather than just tuples.
 *)
type destructor = Project of string | RemoveVariant of string

(*
 * The type for variables
 *)
type var = int

(*
 * branches have an approximation weight, a list of destructors and a variable
 *)
type branch = z_infty * destructor list * var

(*
 * The actual type for term terms ($\mathcal T$ in the
 * paper).
 * Note: the first element of a list of destructors is the rightmost destructor.
 *)
type term =
    Variant of string*term
  | Record of (string*term) list
  | Epsilon of (destructor list)*var
  | Sum of branch list       (* Note: the list should be sorted... *)

(* The exception for impossible cases. *)
exception Impossible_case

(*
 * The type of calls, ie substitutions: just association lists. I
 * could (should?) replace them by maps. At the moment, they are supposed to be
 * sorted and contain elements indexed by 0, 1, ..., n.
 *)
type substitution = (var*term) list
type context = (destructor list * var) list
type call = substitution * context

(* Sets of substitutions *)
module Calls_Set = Set.Make (struct type t=call let compare=compare end)
type calls_set = Calls_Set.t

(* Call graphs: maps indexed by pairs of function names *)
module Call_Graph = Map.Make (struct type t=string*string let compare=compare end)
type call_graph = calls_set Call_Graph.t


(***********************
 * Printing functions. *
 ***********************)

let print_infty = function
    Infty -> print_string "∞"
  | Number(n) -> print_int n

let sub s = String.sub s 1 ((String.length s)-1) (* PML specific *)

let print_destr = function
    RemoveVariant c -> print_string ((sub c)^"-")
  | Project p -> print_string ("π_"^p)

let rec print_list sep pr l = match l with
    [] -> ()
  | [a] -> pr a
  | a::l -> pr a ; print_string sep ; print_list sep pr l

let print_branch b = match b with
    w,ds,i ->
      print_string "<" ;
      print_infty w;
      print_string ">";
      print_list " " print_destr ds ;
      print_string " x_" ; print_int i

let rec print_term = function
    Variant(c,u) ->
      print_string (sub c);
      print_string " ";
      print_term u;
      print_string " "
  | Record [] -> print_string "{ }"
  | Record(l) ->
        print_string "{ ";
    print_list "  ;  " (function label,u -> print_string (label^" = "); print_term u) l;
    print_string " }"
  | Epsilon(ds,i) ->
    List.iter (fun d -> print_destr d; print_string " ") ds;
    print_string "x_"; print_int i
  | Sum(bs) ->
    print_string "( ";
    print_list " + " print_branch bs;
    print_string " )"

let print_substitution tau =
  if tau = [] then print_string "    empty substitution\n";
  List.iter (function i,arg ->
    if i>0 || debugOption "show_lambda"
    then begin
      print_string "    x_"; print_int i;
      print_string " := "; print_term arg;
      print_newline()
    end
  ) tau

let print_context context =
    if debugOption "calling_context" then print_string "      calling context:   {  ";
    print_list "  ,  " (function ds,i -> print_list " " print_destr ds ; print_string " x_"; print_int i) context;
    print_string "  }"

let print_call (tau,context) = print_substitution tau ; print_context context ; print_newline()

let print_graph g =
  let t = ref 0 in
  Call_Graph.iter (fun fg s ->
    let f,g = fg in
    print_string ("  Calls from "^f^" to "^g^":\n");
    Calls_Set.iter (fun tau ->
      t := !t+1;
      print_call tau;
      print_newline()
    ) s
  ) g;
  print_string "  This graph contains "; print_int !t;
  print_string " edge(s).\n"; print_newline()


(**********************************
 * Functions on the z_infty type. *
 **********************************)

(* Collapsing to the set $\{-b,...,0,...,b-1,\infty\}$. *)
let collapse_z_infty b = function
    Infty -> Infty
  | Number(n) -> if (n < -b) then Number (-b)
                 else if (n>=b) then Infty
                 else Number(n)

(*
 * Various helper functions: max, add, less, as well as a function to add a
 * normal integer to an element of the type z_infty
 *)
let max_infty w1 w2 = match w1,w2 with
    Infty,_ | _,Infty -> Infty
  | Number n1, Number n2 -> Number (max n1 n2)
let less_infty w1 w2 = match w1,w2 with
    _,Infty -> true
  | Infty,_ -> false
  | Number n1, Number n2 -> n1 <= n2
let add_infty w1 w2 = match w1,w2 with
    Infty,_ | _,Infty -> Infty
  | Number n1, Number n2 -> Number(n1+n2)
let add_int w n = match w with
    Infty -> Infty
  | Number(m) -> Number(n+m)


(******************************
 * Sanity checking functions. *
 ******************************)

(* checks if a list is sorted *)
let rec sorted l = match l with
    [] -> ()
  | [_] -> ()
  | a::((b::_) as l) when (compare a b)<0 -> sorted l
  | a::b::_ -> assert false

(* checks if a substitution is sorted *)
let sorted_call (tau,context) =
  let rec aux tau n =
    match tau with
        [] -> ()
      | (i,_)::tau when i=n -> aux tau (n+1)
      | _ -> assert false
  in
  aux tau 0 ; sorted context

(* checks if the labels of a record are sorted *)
let rec sorted_labels l = match l with
    [] -> ()
  | [_] -> ()
  | (a,_)::(((b,_)::_) as l) when (compare a b)<0 -> sorted_labels l
  | (a,_)::(b,_)::_ -> print_string ("OUPS: "^a^" / "^b^"\n"); assert false


(***********************
 * Misc list functions *
 ***********************)

(*
 * Get a suffix of length n and return both the suffix and the
 * remaining elements, in reverse order.
 *)
let get_suffix l n =
  let rec aux l n acc =
    if n=0
    then acc,l
    else match l with
        [] -> acc,[]
      | a::l -> aux l (n-1) (a::acc)
  in
  aux (List.rev l) n []

(*
 * Longest common suffix of l1 and l2; returns a triple containing the suffix,
 * and the remainining elements in l1 and l2 in reverse order:
 *    l1 l2  -->  s r1 r2 s.t. l1 = rev_append r1 s
 *                             l2 = rev_append r2 s
 *)
let suffix l1 l2 =
  let rec rev_prefix l1 l2 p = match l1,l2 with
      a::l1, b::l2 when a=b -> rev_prefix l1 l2 (a::p)
    | l1,l2 -> (p,l1,l2)
  in rev_prefix (List.rev l1) (List.rev l2) []

(* Merging two sorted lists, while keeping only one occurence of each element *)
let rec merge l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if h1 < h2
      then h1 :: merge t1 l2
      else if h2 < h1
      then h2 :: merge l1 t2
      else (* if h1=h2 *) h1 :: merge t1 t2

(* Sorting a list and removing keeping only one copy of each element *)
let sort_uniq l =
  let l = List.sort (fun x y -> -(compare x y)) l in
  let rec uniq l acc = match l with
      [] -> acc
    | [a] -> a::acc
    | a::b::l when a=b -> uniq (b::l) acc
    | a::b::l -> uniq (b::l) (a::acc)
  in
  uniq l []


(************************************
 * The approximation order on terms *
 ************************************)

(* approximation for branches of destructors *)
let rec approximates_branch b1 b2 = match b1, b2 with
    (w1,ds1,i1),(w2,ds2,i2) when i1=i2 ->
      begin
        match suffix ds1 ds2 with
            (_,[],ds2') -> less_infty w2 (add_int w1 (List.length ds2'))
          | _ -> false
      end
  | _,_ -> false

(* remove all sequences of destructors that are not maximal for approximates *)
let keep_max l =
  let rec aux1 b l acc = match l with
      [] -> b::acc
    | a::l when approximates_branch a b -> aux1 a l acc
    | a::l when approximates_branch b a -> aux1 b l acc
    | a::l -> aux1 b l (a::acc)
  in
  let rec aux2 l = match l with
      [] -> []
    | b::l -> (aux1 b (aux2 l) [])
  in
  aux2 l


(* computes the normal form of <w>v *)
(* make reduce_approx returns a "Sum [...]" rather than just the list??? *)
let rec reduce_approx w v = match v with
    Variant (_,v) -> reduce_approx (add_int w 1) v
  | Record l -> sort_uniq (keep_max (List.fold_left merge [] (List.map (reduce_approx (add_int w 1)) (List.map snd l))))
  | Sum bs -> sort_uniq (keep_max (List.map (function (w',ds,i) -> (add_infty w w', ds, i)) bs))
  | Epsilon (ds,i) -> [ (w,ds,i) ]


let rec approximates u1 u2 =
  (*
  print_string "approximates with "; print_newline();
  print_term u1 ; print_newline();
  print_term u2 ; print_newline();
  *)
  match u1,u2 with
    Epsilon(ds1,i1), Epsilon(ds2,i2) when ds1=ds2 && i1=i2 -> true
  | Variant(c1,u1), Variant(c2,u2) when c1=c2 -> approximates u1 u2
  | Record l1, Record l2 ->
      let lab1,uu1 = List.split l1 in
      let lab2,uu2 = List.split l2 in
      sorted_labels l1; sorted_labels l2;
      if (lab1=lab2)
      then
        List.for_all2 approximates uu1 uu2
      else false
  | Sum(b1s), Sum(b2s) ->
      List.for_all (fun b2 -> List.exists (fun b1 -> approximates_branch b1 b2) b1s) b2s

  | Sum(_), u2 -> approximates u1 (Sum(reduce_approx (Number 0) u2))

  | _,_ -> false


(* checking if two terms are compatible *)
let rec compatible t1 t2 = match t1,t2 with
    Variant(c1,t1), Variant(c2,t2) when c1=c2 -> compatible t1 t2
  | Record(l1), Record(l2) ->
      let labels1, terms1 = List.split l1 in
      let labels2, terms2 = List.split l2 in
      labels1 = labels2 && List.for_all2 compatible terms1 terms2
  | Sum b1, Sum b2 ->
      List.exists (function b1 ->
      List.exists (function b2 ->
        approximates_branch b1 b2 || approximates_branch b2 b1)
      b2
      )
      b1
  | Epsilon _, Epsilon _ -> t1=t2

  | Sum _, _
  | _, Sum _ -> compatible (Sum(reduce_approx (Number 0) t1))
                           (Sum(reduce_approx (Number 0) t2))

  | _,_ -> false


(*****************************************
 * Flatten the default field of a record *
 * PML specific                          *
 *****************************************)
(* let _Record l = Record(l) *)

(*
 * FIXME: aren't we sure the default field appears as the first field? It seems
 * to be the case in the examples. (See assertion acc=[].)
 *)
let _Record l =
  (*
  let rec flatten_default l acc = match l with
      [] -> raise Exit
    | ("#default",Record[])::_ -> raise Exit
    | ("#default",Record(l))::r -> assert (acc=[]);
        let l = List.filter (function label,_ ->
                  not (List.mem_assoc label r) && not (List.mem_assoc label acc))
        l  (*??FIXME aren't the labels appearing in acc supposed to be shadowed by those in l??*)
        in
        Record(List.rev_append acc (List.rev_append l r))
    | ("#default",_)::_ -> raise Exit
    | r::l -> flatten_default l (r::acc)
  in
  *)
  match l with
    | ["#default",arg] -> arg
    | ("#default",Record[])::_ -> Record l
    | ("#default",Record l)::r ->
        let l = List.filter (function label,_ -> not (List.mem_assoc label r)) l
        in Record(l@r)
    | _ ->
        begin
          if debugOption "additional_sanity_checking"
          then  (match l with _::l -> assert (not (List.mem_assoc "#default" l))
                          | _ -> ())
        end;
        Record l


(******************
 * substitutions. *
 ******************)

(* Get term inside a substitution. *)
(* FIXME: explain why default to Sum[].
 * (Check if this is really what we want with partial applications) *)
let get_term i tau =
  try List.assoc i tau
  with Not_found -> Sum []

(* approximation order for substitutions *)
let approximates_substitution (tau,context1) (sigma,context2) =
  let rec indices a b acc =
    match a,b with
        [],b | b,[] -> List.rev_append (List.rev_map fst b) acc
      | (i,_)::a,(j,_)::b when i=j -> indices a b (i::acc)
      | _ -> assert false
  in
  List.for_all (function i ->
    let v=get_term i sigma in
    let u=get_term i tau in
    approximates u v) (indices tau sigma []) &&
    approximates (Sum (List.rev_map (function ds,i -> (Infty,ds,i)) context1))
                 (Sum (List.rev_map (function ds,i -> (Infty,ds,i)) context2))

(* compatibility for substitutions *)
let compatible_substitution (tau1,_) (tau2,_) =
  try
    List.for_all2 (fun x y -> compatible (snd x) (snd y)) tau1 tau2
  with
    Invalid_argument _ -> assert false (* we only check compatibility for self loops. *)


(* get a specific subtree inside a term: needed for substitution *)
let rec get_subtree ds v = match ds,v with
    [],v -> v
  | ds, Sum(bs) -> Sum(List.map (function w,d,i -> (add_int w (-(List.length ds))),d,i) bs)
  | ds, Epsilon(ds',i) -> Epsilon(List.rev_append ds ds', i)
  | RemoveVariant c::ds, Variant (c',v) when c=c' -> get_subtree ds v
  | RemoveVariant _::_, Variant _ -> raise Impossible_case
  | Project c::ds, Record l ->
    begin
    try
      let v = List.assoc c l in
      get_subtree ds v
    with
    Not_found -> (* if the label "c" isn't found in the record, we instead use
    its default field. Because records are flattened, the default should be a
    set of delta approximations for this to work. Otherwise something is missing
    from the original record... *)
      try
        let v = List.assoc "#default" l in
        get_subtree (Project c::ds) v
      with
        Not_found -> assert false (* typing problem *)
     end
 | (RemoveVariant _::_) as ds, Record l -> (* PML specific *)
    begin
    try
      let v = List.assoc "#default" l in
      get_subtree ds v
    with
      Not_found ->
        (
          print_string "*** TYPING PROBLEM...\n";
          print_string "*** the branch ";
          print_list " " print_destr ds ;
          print_string "\n*** meets the value ";
          print_term v;
          print_newline();
          assert false (* typing problem *)
        )
    end

  | ds,v ->
      (
        print_string "*** TYPING PROBLEM...\n";
        print_string "*** the branch ";
        print_list " " print_destr ds ;
        print_string "\n*** meets the value ";
        print_term v;
        print_newline();
        assert false (* typing problem *)
      )


(*
 * Syntactical substitution in a term, given an environment, with reducing to
 * normal forms.
 *)
let substitute u tau =
  (*
   * Travels inside the term until it finds a variable, preceded
   * by destructors, then uses the above function.
   *)
  let rec red_aux = function
      Variant(c,u) -> Variant(c,red_aux u)
    | Record l ->
        let labels,args = List.split l in
        let args = List.map red_aux args in
        _Record(List.combine labels args)
    | Epsilon(ds,i) ->
        let v = get_term i tau in
        get_subtree (List.rev ds) v
    | Sum(bs) ->
      let bss = sort_uniq (keep_max
        (List.fold_left merge []
          (List.map (function (w,ds,i) -> reduce_approx w (get_subtree (List.rev ds) (get_term i tau))) bs)))
      in
      if bss = []
      then raise Impossible_case
      else Sum bss

  in
    red_aux u


(**************
 * Collapsing *
 **************)

(* Collapsing the weights. *)
let rec collapse1 b u = match u with
    Variant(c,u) -> Variant(c,collapse1 b u)
  | Record l -> ifDebug "additional_sanity_checking" (fun _ -> sorted_labels l);
      let labels, args = List.split l in
      _Record(List.combine labels (List.map (collapse1 b) args)) (* TODO: _Record shouldn't be necessary here *)
  | Epsilon(ds,i) -> Epsilon(ds,i)
  | Sum(bs) ->  ifDebug "additional_sanity_checking" (fun _ -> sorted bs);
      Sum(sort_uniq (keep_max (List.map (function (w,ds,i) -> (collapse_z_infty b w, ds,i)) bs)))


(* Collapsing the destructors. *)
let rec collapse2 d u = match u with
    Variant(c,u) -> Variant(c,collapse2 d u)
  | Record l ->
      let labels, args = List.split l in
      _Record(List.combine labels (List.map (collapse2 d) args)) (* TODO: _Record shouldn't be necessary here *)
  | Epsilon(ds,i) ->
    begin
      let ds',r = get_suffix ds d in
      match r with
          [] -> Epsilon(ds,i)
        | _ -> Sum [ (Number(-(List.length r)),ds',i) ]
    end
  | Sum(bs) ->
    Sum (sort_uniq (keep_max (List.map (function (w,ds,i) ->
      let ds',r = get_suffix ds d in
      (add_int w (-(List.length r)),ds',i)) bs)))


(* Collapsing the constructors. *)
let rec collapse3 d u =
  if d=0
  then
    match u with
        Epsilon _ -> u
      | _ -> Sum (reduce_approx (Number 0) u)
  else
    match u with
        Record l ->
          let labels,args = List.split l in
            _Record (List.combine labels (List.map (collapse3 (d-1)) args)) (* TODO: _Record shouldn't be necessary here *)
      | Variant(c,u) -> Variant(c,collapse3 (d-1) u)
      | _ -> u


(*
 * The three of them together. (It could be slightly improved to prevent
 * traversing the term term three times, but it's probably not worth it.)
 *)
let collapse d b u = collapse1 b (collapse2 d (collapse3 d u))
let collapse_call d b (tau,context) = List.map (function i,u -> i,collapse d b u) tau , context (* FIXME: should we collapse the calling context? *)


(*
 * Composition of substitutions, with collapse. (I don't need the uncollapsed
 * composition in the code.
 *)
let compose d b (tau1,context1) (tau2,context2) =
    (* we check if the calling context of tau2 is compatible with tau1... *)
    List.iter (function ds,i ->
        let t = get_term i tau1 in
        try
            ignore (get_subtree (List.rev ds) t); ()
        with Impossible_case -> 
            (* print_term t; print_string " is incompatible with context "; print_list " " print_destr ds; print_newline (); *)
            raise Impossible_case
    ) context2;
    (* FIXME: should we try to compose the calling contexts? *)
    collapse_call d b ((List.map (function i,u -> i,(substitute u tau1)) tau2),context1)


(*************************
 * Decreasing arguments. *
 *************************)

(* checks if a coherent loop has a decreasing subterm *)
let is_decreasing (tau,_) =

  let isOK ds t i =
    if approximates (Sum [Number(-1), ds, i]) t
    then
    begin
      ifDebug "show_decreasing_terms" begin fun _ ->
        print_string ("** Found decreasing node: **\n");
        print_term (Epsilon(ds,i));
        print_newline();
        print_newline()
      end;
      true
    end
    else false
  in
  (* checks if the i-th term is decreasing *)
  let decr it =
    let i,t = it in
    let rec aux ds t = match t with
        Variant(c,u) ->
          isOK ds t i ||
          aux ((RemoveVariant c)::ds) u
      | Record(l) ->
          isOK ds t i ||
          List.exists (function n,u -> aux ((Project n)::ds) u) l
      | t -> isOK ds t i
    in
    aux [] t
  in
  List.exists decr tau


(*****************************
 * Sets of calls and graphs. *
 *****************************)

(*
 * Adding a call to a set, keeping only maximal elements for the approximation
 * order.
 * It would be better if we could do this at the same time as the following
 * function: try to add the element, and raise an exception if it already
 * appears (or an approximation appears). This requires to use a custom map
 * module...
 *)
let add_call_set tau s =
  ifDebug "additional_sanity_checking" (fun _ -> sorted_call tau);
  if (debugOption "use_approximates")
  then
    if Calls_Set.exists (fun sigma -> approximates_substitution sigma tau) s
    then s
    else Calls_Set.add tau (Calls_Set.filter (fun sigma -> not (approximates_substitution tau sigma)) s)
  else
    Calls_Set.add tau s

(* Checks if a call brings new information. *)
let new_call_set tau s =
    if (debugOption "use_approximates")
    then not (Calls_Set.exists (fun sigma -> approximates_substitution sigma tau) s)
    else not (Calls_Set.mem tau s)  (* FIXME something might be wrong here *)

(* Counts the number of calls in a graph.  *)
let count_edges g = Call_Graph.fold (fun _ s n -> n+(Calls_Set.cardinal s)) g 0

(* Computing the transitive closure of a graph. *)
let transitive_closure initial_graph d b =

  (* references to keep some info about the graph *)
  let new_arcs = ref false in
  let nb_steps = ref 0 in

  (* single step for the TC. "ig" is the initial graph, "g" is the current graph *)
  let one_step_TC ig g =
    (*
     * local reference for the resulting graph: this is initialized to the
     * starting graph
     *)
    let result = ref g in

    Call_Graph.iter (fun fg a ->
      Call_Graph.iter (fun fg' a' ->
        let f,g = fg in
        let f',g' = fg' in
        if g=f'
        then begin
          Calls_Set.iter (fun tau ->
            Calls_Set.iter (fun tau' ->
              let all_calls = try Call_Graph.find (f,g') !result
                              with Not_found -> Calls_Set.empty
              in
              try
                ifDebug "show_all_compositions"
                begin fun _ ->
                  print_string "** Composing: **\n";
                  print_call tau;
                  print_string "    with\n";
                  print_call tau';
                  print_string "    with B="; print_int b; print_string " and D="; print_int d; print_string "\n** to give\n";
                end;
                let sigma : call = compose d b tau tau' in
                ifDebug "show_all_compositions"
                begin fun _ ->
                  print_call sigma;
                  print_newline();
                  print_newline()
                end;
                if (new_call_set sigma all_calls)
                then begin
                  new_arcs := true;
                  result := Call_Graph.add (f,g') (add_call_set sigma all_calls) !result;
                end
              with Impossible_case ->
                ifDebug "show_all_compositions"
                begin fun _ ->
                  print_string "    IMPOSSIBLE CASE...";
                  print_newline();
                  print_newline()
                end;
            ) a'
          ) a
        end
      ) ig
    ) g;
    !result
  in

  (*
   * Computing the actual closure. We know we've reached the TC when no new
   * edge was added.
   *)
  let rec closure ig g =
    new_arcs := false;
    ifDebug "show_all_steps"
    begin fun _ ->
      print_string ("** Graph of paths at iteration "^(string_of_int (!nb_steps))^" **\n");
      print_graph g;
      print_newline()
    end;
    let g = one_step_TC ig g in
    if not !new_arcs
    then g
    else begin
      nb_steps := !nb_steps+1;
      closure ig g
    end
  in

  (* collapse all substitutions *)
  ifDebug "show_initial_call_graph"
  begin fun _ ->
    print_string "** Control-flow graph given by the static analysis: **\n";
    print_graph initial_graph
  end;
  ifDebug "initial_collapse_of_graph"
  begin fun _ ->
  let initial_graph = Call_Graph.map (fun s ->
                Calls_Set.fold (fun tau s ->
                  add_call_set (collapse_call d b tau) s)
                  s Calls_Set.empty)
                  initial_graph in
  ifDebug "show_initial_call_graph"
  begin fun _ ->
    print_string "** Control-flow graph after collapse: **\n";
    print_graph initial_graph
  end
  end;
  let graph_of_paths = closure initial_graph initial_graph in

  ifDebug "show_final_call_graph"
  begin fun _ ->
    print_string "** Graph of paths of the initial control-flow graph: **\n";
    print_graph graph_of_paths
  end;
  ifDebug "show_summary_TC"
  begin fun _ ->
    print_string "* the initial control-flow graph contained "; print_int (count_edges initial_graph); print_string " edge(s) *\n";
    print_string "* its graph of paths contains "; print_int (count_edges graph_of_paths); print_string " edge(s) *\n";
    print_string "* "; print_int !nb_steps; print_string " iteration(s) were necessary to compute the graph of paths. *\n";
    print_newline()
  end;

  (* Returns the value of the TC. *)
  graph_of_paths


(*
(* computes the list of function names (nodes) that are accessible from f in the
 * given graph *)
let accessible_from graph f =
  let seen = ref [] in
  let rec dfs n =
    assert (not (List.mem n !seen));
    seen := n::!seen;
    let next = Call_Graph.fold
                (fun xy s nxt ->
                   let x,y = xy in
                   if n = x && (not (Calls_Set.is_empty s)) && (not (List.mem y !seen))
                   then y::nxt
                   else nxt)
                graph
                []
    in
    List.iter dfs next
  in
  !seen
*)

(**********************************************************************
 * Putting everything together: the size-change termination principle *
 **********************************************************************)
let remove_contexts graph =
  let newgraph = ref Call_Graph.empty in
    Call_Graph.iter (fun fg a ->
        Calls_Set.iter (function tau,_ ->
            let s = try Call_Graph.find fg !newgraph
              with Not_found -> Calls_Set.empty in
            newgraph := Call_Graph.add fg (add_call_set (tau,[]) s) !newgraph
            ) a
        ) graph;
  !newgraph


let size_change_termination_bounds graph d b =
  assert (d>=0 && b>0) ;
  let tc_graph = transitive_closure graph d b in
    Call_Graph.for_all
      (fun fg s ->
        let f,g = fg in
        f<>g ||
        Calls_Set.for_all
          (fun sigma ->
            try
              not (compatible_substitution sigma (compose d b sigma sigma)) ||
              begin
                ifDebug "show_coherents"
                begin fun _ ->
                  print_string ("** Found coherent loop from \"" ^ f ^ "\" to itself: **\n");
                  print_call sigma
                end;
                is_decreasing sigma ||
                (ifDebug "show_nondecreasing_coherents" begin fun _ ->
                  print_string ("** Found non-decreasing coherent loop from \"" ^ f ^ "\" to itself: **\n");
                  print_call sigma;
                  print_newline()
                end;
                false)
              end
            with Impossible_case -> true
          ) s
      ) tc_graph


(*****************************************
 * The functions called from the outside *
 *****************************************)

let size_bound = ref 1
let depth_bound = ref 2

let size_change_termination graph =

  let graph = if debugOption "use_calling_context"
              then graph
              else remove_contexts graph in

  let rec ds n acc =
    if (!depth_bound <= n)
    then List.rev (!depth_bound::acc)
    else ds (2*n) (n::acc)
  in
  let rec test = function
      [] -> false
    | d::ds ->
        ifDebug "show_summary_TC"
        begin fun _ ->
          print_string "** Incremental test: using d = ";
          print_int d;
          print_string " **";
          print_newline()
        end;
        let t = size_change_termination_bounds graph d !size_bound in
        if t
        then (
          ifDebug "show_summary_TC"
          begin fun _ ->
            print_string "** These functions are size change terminating for d=";
            print_int d; print_string " and b=";
            print_int !size_bound;print_string ". **\n\n"
          end;
          true)
        else
          test ds
  in
  let t = test (ds 1 [0]) in
  if t
  then true
  else (
    ifDebug "show_summary_TC"
    begin fun _ ->
      print_string "** These functions are NOT size change terminating for d=";
      print_int (!depth_bound); print_string " and b=";
      print_int (!size_bound);print_string ". **\n\n"
    end;
    false)

