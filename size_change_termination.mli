(* misc tools *)
val sort_uniq : 'a list -> 'a list

(* Debug options *)
val debugOptions : (string * bool) list ref
val setOption : string -> bool -> unit
val debugOption : string -> bool
val ifDebug : string -> (unit -> unit) -> unit

(* types *)
type z_infty = Number of int | Infty
type var = int
type destructor = Project of string | RemoveVariant of string
type branch = z_infty * destructor list * var
type term =
    Variant of string * term
  | Record of (string * term) list
  | Epsilon of destructor list * var
  | Sum of (z_infty * destructor list * var) list
exception Impossible_case

type substitution = (var*term) list
type context = (destructor list * var) list
type call = (var*term) list * (destructor list * var) list
module Calls_Set :
  sig
    type elt = call
    type t
    val empty : t
  end
type calls_set = Calls_Set.t
module Call_Graph :
  sig
    type key = string * string
    type +'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
  end
type call_graph = calls_set Call_Graph.t

(* printing functions *)
val print_infty : z_infty -> unit
val print_destr : destructor -> unit
val print_list : string -> ('a -> unit) -> 'a list -> unit
val print_branch : branch -> unit
val print_term : term -> unit
val print_context : context -> unit
val print_substitution : substitution -> unit
val print_call : call -> unit
val print_graph : Calls_Set.t Call_Graph.t -> unit

(* sanity check functions *)
val sorted_call : call -> unit
val sorted_labels : (string * term) list -> unit

(* operations on z_infty *)
val collapse_z_infty : int -> z_infty -> z_infty
val max_infty : z_infty -> z_infty -> z_infty
val less_infty : z_infty -> z_infty -> bool
val add_infty : z_infty -> z_infty -> z_infty
val add_int : z_infty -> int -> z_infty

val approximates_branch : branch -> branch -> bool
val keep_max : branch list -> branch list
val reduce_approx : z_infty -> term -> branch list
val approximates : term -> term -> bool
val compatible : term -> term -> bool

val approximates_substitution : call -> call -> bool
val compatible_substitution : Calls_Set.elt -> call -> bool
val get_subtree : destructor list -> term -> term
val substitute : term -> substitution -> term
val collapse1 : int -> term -> term
val collapse2 : int -> term -> term
val collapse3 : int -> term -> term
val collapse : int -> int -> term -> term
val collapse_call : int -> int -> call -> call
val compose : int -> int -> call -> call -> call
val is_decreasing : call -> bool
val add_call_set : Calls_Set.elt -> Calls_Set.t -> Calls_Set.t
val new_call_set : Calls_Set.elt -> Calls_Set.t -> bool
val count_edges : Calls_Set.t Call_Graph.t -> int
val transitive_closure : Calls_Set.t Call_Graph.t -> int -> int -> Calls_Set.t Call_Graph.t
val size_change_termination_bounds : Calls_Set.t Call_Graph.t -> int -> int -> bool

val size_bound : int ref
val depth_bound : int ref
val size_change_termination : Calls_Set.t Call_Graph.t -> bool

