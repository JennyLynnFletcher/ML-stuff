(*---8.1---*)

(*
This function reverses the order of function application from the order the functions x and y are sent as arguments. This allows partial function application of f y x, where x and f are specified only and y is passed in at a later stage. 
*)
    


(*---8.2---*)

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

(*---8.3---*)

fun map2 f [] = []
    | map2 f ([]::xss) = []::(map2 f xss)
    | map2 f ((x::xs)::xss) =
        let val y::ys = map2 f (xs::xss)
        in ((f x)::y)::ys
        end;
    
    
(* ---8.4---*)

datatype 'a option = NONE | SOME of 'a;

fun mapo _ NONE = NONE
    | mapo f (SOME x) = SOME (f x);

    
(*---9.1---

datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);

fun map f (Cons(x, xf)) = 
    Cons(x, xf); *)
    
(*---9.2---*)

(* Concatenating sequences where any sequence but the last is an infinite sequence is not possible. As for finite sequences concat would produce a function to generate the next term in the sequence that uses the functions provided by the sequences to be concatenated within given ranges, where the range of the second sequence starts at the term where the first sequence produces a Nil value. However if an infinite sequence passed to a concat function as anything other than the last sequence to be concatenated, there is no Nil term for the range of the next sequence to start *)


(*-96-1-2-*)

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

(*---6.4---*)

datatype E =  real
            | VAR of string
            | COMPLEMENT of E
            | ADD of E*E
            | MULTIPLY of E*E;


(*-11-1-2-*)

(*exception Olive;

fun test x = if x = 1 then "Fine" else (raise Olive)
                handle Olive => "Olive Exception Raised";

fun cannot f x = f x = "Olive Exception Raised";

datatype 'a option = NONE | SOME of 'a;

datatype 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree;


fun bun (x, Leaf y) = if x=y then NONE else SOME (Leaf y)
    | bun (x, SOME (Branch (t1, t2))) = SOME (Branch (bun(x, t1),bun(x, t2)));


fun cheese (x,t) = if bun (x, SOME t) = NONE then Leaf x else bun(x, SOME t);*)

(*-13-1-1-*)

datatype 'a tree = Leaf | Br of ('a * 'a tree * 'a tree);

fun alltrees _ [] [] = []
    | alltrees label (lt::lts) [] = []
    | alltrees label [] (rt::rts) = []
    | alltrees label (lt::lts) (rt::rts) = Br (label, lt, rt)::(alltrees label lts (rt::rts))@(alltrees label (lt::lts) rts)@(alltrees label (lt::lts) (rt::rts));

fun inorder Leaf = []
    | inorder (Br (v, lt, rt)) = (inorder lt)@[v]@(inorder rt);
    
    
(*-15-1-1-*)

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
    
    
(*-15-1-2-*)

exception OutofIndex

datatype 'a lazylist = Nil | Cons of 'a * (unit -> 'a lazylist);

fun tail (Cons(_,xf)) = xf();

fun from n () = Cons (n, from (n + 1));

val posints = from 1 ();

fun returnElement n (Cons (v, f)) = if n = 1 then v
                                    else returnElement (n-1) (f())
    | returnElement _ Nil = raise OutofIndex;















