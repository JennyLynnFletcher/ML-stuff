(* 2017.1.1 *)

(* a *)

(* 

exists returns false if there are no next states, otherwise returns true if the first element fulfills the
predicate, if not it then recurses on itself with the tail of the list, until the list is empty or an element 
that fulfills the predicate is found.

Winnable is true if the current state is the winnig state (winning x = true) or there exists an element in next
x that is winnable. So to check if a state is winnable, a depth first search of the states is used until a
winning state is found.

Because a depth first search is used, if it is impossible for a winning state to be reach from a given state,
and the next states will never terminate, then the depth first search will explore down the state from which it
is unwinnable forever, and will never check any of the other states that may be winnable

*)

fun winning x = x = 20;

fun next x = [x + 3, x + 4];

fun exists p [] = false
    | exists p (x::xs) = p x orelse exists p xs;
    
fun winnable x = winning x orelse exists winnable (next x);

(* winnable 1 will never terminate *)

(* b *)

exception none_winnable

fun next_winnable [] = raise none_winnable
    | next_winnable (x::xs) = if winnable x then x else next_winnable xs;

fun winpath x = if winning x then [x] else if winnable x then x::winpath(next_winnable (next x)) else [];
                
(* c *)

exception path_greater_than_bound;

fun bounded_winpath_h  0 x _ = if winning x then [x] else raise path_greater_than_bound
    | bounded_winpath_h _ _ [] = []
    | bounded_winpath_h n x (s::ss) = if winning x then [x] else
                            x::(bounded_winpath_h (n-1) (next_winnable (next x)) (next x))
                            handle path_greater_than_bound => bounded_winpath_h (n-1) (next_winnable ss) ss
                                handle none_winnable => [];

fun bounded_winpath n x = bounded_winpath_h n x (next x);

(*
Doesn't work, but I feel I might be along the right lines. Feels like I might be missing a base case somewhere
as non-termination for all but inputs where bounded path possible using only the first in the list of next x
for all steps.
*)

(* d *)

fun new_winpath_h n x = if not (bounded_winpath n x = []) then bounded_winpath n x else new_winpath_h (n + 1) x;
fun new_winpath x = new_winpath_h 1 x;

(*
Uses a breadth first search, so will check all shorter paths before any longer path, however at each increase
of depth of path, all paths need to be recomputed from the beginning, making this slow 
*)

(* e *)

(*
Using a queue to store each state to be explored later, exploring all states that are only one state away from
the current state before exploring any that are more than one state away. This reduces time complexity, however
requires a large amount of space (increases exponentially as depth increases))
*)
