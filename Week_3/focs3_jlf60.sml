(* Jenny Fletcher,  jlf60
 * FoCS, Supervision 3
 * DD/MM/YYYY HH:MM *)

(* ------------------ *)
(* 1: Notes 8.1 - 8.4 *)
(* ------------------ *)

(* 8.1: Explain fun sw f x y = f y x; *)
(* ANSWER *)

(*
This function reverses the order of function application from the order the functions x and y are sent as arguments. This allows partial function application of f y x, where x and f are specified only and y is passed in at a later stage. 
*)

(* 8.2: Lexicographic ordering *)
(* ANSWER *)

fun pairComparison orderx ordery (xa, ya) (xb, yb) = 
    (orderx xa xb) orelse (xa = xb andalso (ordery ya yb));
    
fun lessthan x y = x < y;

fun stringlessthan x y = String.compare(x, y) = LESS;

fun merge [] ys = ys
    | merge xs [] = xs
    | merge ((xa, ya)::xs) ((xb, yb)::ys) =
        if pairComparison lessthan lessthan (xa, ya) (xb,yb) then (xa, ya)::(merge xs ((xb, yb)::ys))
        else (xb, yb)::(merge ((xa, ya)::xs) ys);
        
(*
pairComparison can be used in insort instead of < passing in the arguments of how you want the pair elements to be compared, as well as the two pairs. For example passing in a pair of type (int * string) you would ohave to pass in lessthan and stringlessthan to sort them in ascending order first on the int then the string
*)

(* 8.3: map2 f = map (map f) using nested recursion or pattern matching *)
(* ANSWER *)

fun map2 f [] = []
    | map2 f ([]::xss) = []::(map2 f xss)
    | map2 f ((x::xs)::xss) =
        let val y::ys = map2 f (xs::xss)
        in ((f x)::y)::ys
        end;

(* 8.4: mapOpt: ('a -> 'b) -> 'a option -> 'b option *)
(* ANSWER *)

datatype 'a option = NONE | SOME of 'a;

fun mapo _ NONE = NONE
    | mapo f (SOME x) = SOME (f x);

(* ------------------ *)
(* 2: Notes 9.1 - 9.2 *)
(* ------------------ *)

(* 9.1: mapSeq *)
(* ANSWER/STUCK/SKIPPED *)

(* 9.2: concatSeq *)
(* ANSWER *)

(* Concatenating sequences where any sequence but the last is an infinite sequence is not possible. As for finite sequences concat would produce a function to generate the next term in the sequence that uses the functions provided by the sequences to be concatenated within given ranges, where the range of the second sequence starts at the term where the first sequence produces a Nil value. However if an infinite sequence passed to a concat function as anything other than the last sequence to be concatenated, there is no Nil term for the range of the next sequence to start *)

(* --------------------------------------------- *)
(* 3: Pick two of: 1996.1.2, 6.3-6.4, 1998.13.10 *)
(* --------------------------------------------- *)

(* 1996.1.2: Boolean expression evaluator *)
(* ANSWER *)

datatype BOOL =   VAR of string
                | NOT of BOOL
                | AND of BOOL*BOOL
                | OR  of BOOL*BOOL
                
fun   distinctname (VAR boolexpr) = [boolexpr]
    | distinctname (NOT boolexpr)  = distinctname boolexpr
    | distinctname (AND (boolexpra, boolexprb)) = (distinctname boolexpra )@(distinctname boolexprb)
    | distinctname (OR (boolexpra, boolexprb)) = (distinctname boolexpra)@(distinctname boolexprb);
    
fun   member _ [] = false
    | member v (x::xs) = if v = x then true else member v xs; 
    
fun   evaluate (VAR boolexpr) context = member boolexpr context
    | evaluate (NOT boolexpr) context = not (evaluate boolexpr context)
    | evaluate (AND (boolexpra, boolexprb)) context = (evaluate boolexpra context) andalso (evaluate boolexprb context)
    | evaluate (OR  (boolexpra, boolexprb)) context = (evaluate boolexpra context) orelse (evaluate boolexprb context);

fun   combinationContext [] = []
    | combinationContext [x] = [[],[x]]
    | combinationContext (x::xs) = let val subset = combinationContext xs
                                   in (map (fn z => x::z) subset) @ subset
                                   end;
                                   
fun alltruth boolexpr = let val contexts = combinationContext (distinctname boolexpr)
                        fun   subeval _ [] = true
                            | subeval boolexpr (x::xs) = evaluate boolexpr x andalso subeval boolexpr xs
                            in subeval boolexpr contexts
                            end;

(* 6.3-6.4: Arithmetic expression evaluator *)
(* ANSWER *)

(* 

6.3

Br(1, ftree(2, n-1), ftree(3, n-1))
Br(1, Br(2, ftree(4, n-2), ftree(5, n-2)), Br(3, ftree(6, n-2), ftree(7, n-2)))

Creates a balanced binary tree of depth n that will produce the sequence [1,2,3,4,....,2^n] when an inorder traversal is used
*)

(*6.4*)

datatype E =  real
            | VAR of string
            | COMPLEMENT of E
            | ADD of E*E
            | MULTIPLY of E*E;

(* 1998.13.10: Imperative language evaluator *)
(* SKIPPED *)

(* ---------------------------------------------------------------- *)
(* 4: Difficulty writing datatype for sentence grammar from Week 1. *)
(* ---------------------------------------------------------------- *)
(* ANSWER *)

(* 
(Presumably apart from the lack of modal verb) 
Datatypes need to be defined for NP1, NP2, PP, P, V and VP before one for S can be defined, as S is not defined in terms of itself, but is instead defined in terms of sub-datatypes. This also causes problems for NP1, V and P, which are defined as a list of words, which may want to be changed at a later date, or for a given context, so it would be preferable to have the options for NP1, V and P be variable from an input list instead of unchangeable
*)

(* ------------ *)
(* 5: Tripos Qs *)
(* ------------ *)

(* ---------------------------------------- *)
(* 2011.1.2: Exceptions, Olive, bun, cheese *)
(* ---------------------------------------- *)

(* a: Exceptions and control flow *)
(* ANSWER *)

(*
 (Not quite sure what the question is asking, but having a go anyway)
-Exceptions are an event which can be caused to prevent programs from crashing, and they are able to be                returned from a function regardless of the type of that function. 

-If not handled they halt the program, however if handled allow specifc code to be run in the case that a specific excpetion has been raised, so if exception A is raised a different piece of handler code is called than if excpetion B is raised

-They provide an output to the user which can specify the sort of error encountered (e.g divide by 0), giving insight as to why the inputs are causing problems

-To raise an exception instead giving a return value raise [exception name] can be used

-To define handler use code handle [exception name] => fn where fn is the function to be called

-the handler code for exceptions can be defined later in the code than where the exception is raised
s 
*)

(* b: cannot *)
(* STUCK *)

(* Only very specific case of exception being raised. Don't know how to generalise *)
exception Olive;

fun test x = if x = 1 then "Fine" else (raise Olive)
                handle Olive => "Olive Exception Raised";

fun cannot f x = f x = "Olive Exception Raised";


(* c.i: Type of cheese  *)
(* ANSWER *)

(*
bun fn: (''a * ''a tree) -> ''a tree

cannot(bun (x,t)) : (''a* ''a tree) -> ''a tree -> bool

fun cheese (x,t) = if cannot(bun,(x,t)) then                     Leaf x else bun(x,t)
(''a * ''a tree)      ((''a* ''a tree) -> ''a tree -> bool)      ''a tree

cheese fn: (''a * ''a tree) -> ''a tree
*)

(* c.ii: Equivalent to cheese but no exceptions *)
(* STUCK *)

(* know should do it using options, but can't get types to quite match up*)

(*
datatype 'a option = NONE | SOME of 'a;

datatype 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree;


fun bun (x, Leaf y) = if x=y then NONE else SOME (Leaf y)
    | bun (x, SOME (Branch (t1, t2))) = SOME (Branch (bun(x, t1),bun(x, t2)));


fun cheese (x,t) = if bun (x, SOME t) = NONE then Leaf x else bun(x, SOME t);
*)

(* ---------------------------- *)
(* 2013.1.1: All possible trees *)
(* ---------------------------- *)

(* a *)
(* ANSWER *)

(*
-Datatypes in ML are a mechanism for user defined types

-Can be defined as a primitive type, datatype or a operation on another type or combination of types, including the datatype being defined, giving a recursive datatype

-As with other types they can be tested for equality and passed in as arguments

-Pattern matching allows different code to be run depending on the form of the data passed into a function

-Uses the pipe | to split up different patterns

-This can also be used in datatype declarations to give multiple different forms of a datatype
*)


(* b *)
(* ANSWER *)

datatype 'a tree = Leaf | Br of ('a * 'a tree * 'a tree);

fun alltrees _ [] [] = []
    | alltrees label (lt::lts) [] = []
    | alltrees label [] (rt::rts) = []
    | alltrees label (lt::lts) (rt::rts) = Br (label, lt, rt)::(alltrees label lts (rt::rts))@(alltrees label (lt::lts) rts)@(alltrees label (lt::lts) (rt::rts));

(* c *)
(* STUCK *)

(*
Reverse list and create a list of all trees with the first element of the reversed list as the root, and either with the second element as the root of the left subtree or witht the left subtree being a leaf. This is repeated for all elements of the list
*)

(* Still working out exactly how to get this to work *)

(* --------------------------- *)
(* 2015.1.1: Functional arrays *)
(* --------------------------- *)

(* a *)
(* ANSWER *)

(*
- In a functional array each element is stored as a node in a balanced binary tree, with a subscript that is equivalent to the placement in the tree

-At each node of the tree a pair is stored, made up of the subscript and the value being stored, each node also points to a left and a right subtree

- Because subscripts are in a fixed ordered position, this means that because search area is halved each time a node is inspected, it takes O(logn) time to find an element in the average case, this is the same for the update function as the update function will find the node corresponding to the value to be updated the same way as described above, and then the value is changed.
*)

(* b *)
(* ANSWER *)

datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

exception Empty

fun tcons v Lf = Br (v, Lf, Lf)
      | tcons v (Br (w, t1, t2)) = Br (v, tcons w t2, t1);
      
fun arrayoflist [] = Lf
    | arrayoflist (x::xs) = tcons x (arrayoflist xs);


(* c *)
(* ANSWER *)
fun root Lf = raise Empty
    | root (Br (v, lt, rt)) = v;
    
fun traverse (Br (v, Lf, Lf)) = Lf
    | traverse (Br (v, lt, rt)) = Br ((root lt), rt, (traverse lt))
    | traverse Lf = raise Empty;

fun subsOfTrueForH f Lf _ = []
    | subsOfTrueForH f (Br (v, lt, rt)) x = 
        if f v then x::(subsOfTrueForH f (traverse (Br (v, lt, rt))) (x+1))
        else subsOfTrueForH f (traverse (Br (v, lt, rt))) (x+1);
        
fun subsOfTrueFor f Lf = []
    | subsOfTrueFor f (Br (v, lt, rt)) = subsOfTrueForH f (Br (v, lt, rt)) 1;

(* ------------------------------ *)
(* 2015.1.2: Lazy lists, zipWith2 *)
(* ------------------------------ *)

(* a: Lazy list notes *)
(* ANSWER *)

(*
-Lazy lists are lists where each value is calculated from a function that is part of the definition of a lazy list, this means the tail of a lazy list doesn't have to be calculated until the tail is requested. This also means that lists do not need to be of finite length, as it isn't required for all of the elements to ever be calculated

infinite lazy list has type 'a * (unit -> ('a * (unit -> ('a * (unit -> (..........))))))
which can be defined recursively as 'a lazylist = ('a * (unit -> 'a lazylist))
*)

exception OutofIndex

datatype 'a lazylist = Nil | Cons of 'a * (unit -> 'a lazylist);

fun tail (Cons(_,xf)) = xf();

fun from n () = Cons (n, from (n + 1));

val posints = from 1 ();

fun returnElement n (Cons (v, f)) = if n = 1 then v
                                    else returnElement (n-1) (f())
    | returnElement _ Nil = raise OutofIndex;

(* b: diag *)
(* STUCK *)

(* c: zipWith2-ish *)
(* SKIPPED *)

(* d: seq seq to seq *)
(* SKIPPED *)

(* ---------------------------------------------------- *)
(* 2013.1.2, b-c: Permutations (just the laziness bits) *)
(* ---------------------------------------------------- *)

(* b: lperms not lazy enough *)
(* SKIPPED *)

(* c: Fix lperms *)
(* SKIPPED *)

