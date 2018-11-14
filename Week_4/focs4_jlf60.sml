(* Jenny Fletcher,  jlf60
 * FoCS, Supervision 4
 * 3/11/2018 17:00 *)


(* 2017.1.1 *)

(* a *)

(* 
exists returns false if there are no next states, otherwise returns true if the 
first element fulfills the predicate, if not it then recurses on itself with the 
tail of the list, until the list is empty or an element that fulfills the 
predicate is found.

Winnable is true if the current state is the winnig state (winning x = true) or 
there exists an element in next x that is winnable. So to check if a state is 
winnable, a depth first search of the states is used until a winning state is 
found.

Because a depth first search is used, if it is impossible for a winning state to 
be reach from a given state, and the next states will never terminate, then the 
depth first search will explore down the state from which it is unwinnable 
forever, and will never check any of the other states that may be winnable
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

fun winnable_in n x = if n = 0 then winning x else winning x orelse exists (winnable_in (n-1)) (next x);

fun next_winnable_in n [] = raise none_winnable
    | next_winnable_in n (x::xs) = if winnable_in n x then x else next_winnable_in n xs;

fun bounded_winpath n x = if winning x then [x] else 
                          if winnable_in n x then x::(bounded_winpath (n-1) (next_winnable_in n (next x)))
                          else [];

(* d *)

fun new_winpath_h n x = if not (bounded_winpath n x = []) then bounded_winpath n x 
                        else new_winpath_h (n + 1) x;
                        
fun new_winpath x = new_winpath_h 1 x;

(*
Uses a breadth first search, so will check all shorter paths before any longer 
path, however at each increase of depth of path, all paths need to be recomputed 
from the beginning, making this slow 
*)

(* e *)

(*
Using a queue to store each state to be explored later, exploring all states 
that are only one state away from the current state before exploring any that 
are more than one state away. This reduces time complexity, however requires a 
large amount of space (increases exponentially as depth increases))
*)


(* 2011.1.1 *)

(* a *)

(*
The labyrinth can be represented as a graph (stored as an adjacency list) where 
each node on the graph is representing a grid position, and each arc connects 
two nodes which are non-diagonally adjacent and have no wall inbetween them.
*)

(* taking only a 2x2 section of the given graph for testing as I'm lazy *)

val lab_def = [((0,0),(1,0)),((0,0),(0,1)),((1,0),(0,0)),((0,1),(0,0))]

fun   next _ [] = []
    | next pos (l::ls) = if pos = l then pos::(next pos ls) else next pos ls;

(* b *)

fun pos_path fin current = current = fin orelse exists (pos_path fin) (next current lab_def); 

(* needs to be forced to terminate I think, maybe use depth restriction of the number of cells in the labyrinth? *)

(* c *)

(*
Using a breadth first search the first path found between two positions will be 
the shortest. This is also the case for iterative deepening. A breadth first 
search is more time efficient, however has a space efficiency of O(2^n) where n 
is the depth of the shortest path, as each subtree of every node visited needs 
to be added to a queue. This is not space efficient. Iterative deepening is less 
time efficient, as it recomuptes sections of paths multiple times, however is 
more space efficient. Iterative deepening performs a bounded depth first search, 
to a given depth, and if the path is found it is returned, however if not it is 
repeated with the depth incremented. This means that at the worst point in the 
algorithm, the space used is a list of length equal to the depth of the shortest 
path so O(n) where n is the depth.
*)


(* 1997.1.5 *)

(* a *)

datatype 'a tree = Nil | Br of 'a * 'a tree list;

datatype Token = X | O | Empty;

(* b *)

val start_board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty];

fun   winning_state [O, O, O, _, _, _, _, _, _] = true
    | winning_state [X, X, X, _, _, _, _, _, _] = true
    | winning_state [_, _, _, O, O, O, _, _, _] = true
    | winning_state [_, _, _, X, X, X, _, _, _] = true
    | winning_state [_, _, _, _, _, _, O, O, O] = true
    | winning_state [_, _, _, _, _, _, X, X, X] = true
    | winning_state [O, _, _, O, _, _, O, _, _] = true
    | winning_state [X, _, _, X, _, _, X, _, _] = true
    | winning_state [_, O, _, _, O, _, _, O, _] = true
    | winning_state [_, X, _, _, X, _, _, X, _] = true
    | winning_state [_, _, O, _, _, O, _, _, O] = true
    | winning_state [_, _, X, _, _, X, _, _, X] = true
    | winning_state [O, _, _, _, O, _, _, _, O] = true
    | winning_state [X, _, _, _, X, _, _, _, X] = true
    | winning_state [_, _, O, _, O, _, O, _, _] = true
    | winning_state [_, _, X, _, X, _, X, _, _] = true
    | winning_state _ = false;
    

fun   next_board_list [] _ _ = []
    | next_board_list (t::ts) token xs = if t = Empty then (xs@(token::ts))::(next_board_list ts token (xs@[t]))
                                         else (next_board_list ts token (xs@[t]));
                                        

fun   mkbranch _ [] _ = []
    | mkbranch token (t::ts) xs = let val next_token = if token = X then O else X in
                                  if winning_state (t::ts) then [Br((t::ts), [])] 
                                  else if t = Empty then (Br((xs@(next_token::ts)), mkbranch next_token (xs@[next_token]@ts) []))::(mkbranch token ts (xs @ [t]))
                                  else mkbranch token ts (xs @ [t])
                                  end;
fun mktree () = Br(start_board, mkbranch O start_board []);
                                     
(* c *)

(* 
mktree uses append often, it could be improved by using :: and only reversing the list when it needs to be used
*)


(* d *)

fun   O_winning [O, O, O, _, _, _, _, _, _] = true
    | O_winning [_, _, _, O, O, O, _, _, _] = true
    | O_winning [_, _, _, _, _, _, O, O, O] = true
    | O_winning [O, _, _, O, _, _, O, _, _] = true
    | O_winning [_, O, _, _, O, _, _, O, _] = true
    | O_winning [_, _, O, _, _, O, _, _, O] = true
    | O_winning [O, _, _, _, O, _, _, _, O] = true
    | O_winning [_, _, O, _, O, _, O, _, _] = true
    | O_winning _ = false;

(* 
use an exists function for a tree with an accumulator, taking O_winning as the predicate
*)

fun   count_tree p (Br(board, [])) = if p board then 1 else 0
    | count_tree p (Br(board, (t::ts))) = if p board then ( 1 + (count_tree p (Br(board, (ts)))) + (count_tree p t) )
                                        else (count_tree p (Br(board, (ts)))) + (count_tree p t);

fun Owins x = count_tree O_winning x;



(* 2008.1.6 *)

(* a *)

datatype 'a puzzle =  Puzzle of (('a -> 'a list)*('a -> bool)); 

(* b *) 

(*
Breadth first search, searches all roots of current subtrees, before exploring 
the leaves of the current subtrees. Depth first search searches all the whole of 
a right subtree before searching any of the left subtree. 
Iterative deepening performs a depth first search to a given depth, then if not 
found increments the depth until found
*)

(* c *)

fun depth (Puzzle(next_mv, win)) n x = if n = 0 then win x else win x orelse exists (depth (Puzzle(next_mv, win)) (n-1)) (next_mv x);


(* d *)

fun   explore (Puzzle(next_mv, win)) pos [] = []
    | explore (Puzzle(next_mv, win)) pos (m::ms) = ms@(next_mv pos);

fun   breadth_h (Puzzle(next_mv, win)) x [] = win x
    | breadth_h (Puzzle(next_mv, win)) x (m::ms) = if win x then true else breadth_h (Puzzle(next_mv, win)) x (explore (Puzzle(next_mv, win)) m (m::ms));
    
fun breadth puz x = breadth_h puz x [];











































