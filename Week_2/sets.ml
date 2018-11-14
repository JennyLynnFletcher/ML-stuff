datatype 'a set = Nil | Cons of 'a * 'a set;

val empty = Nil;

fun member (_, Nil)  = false
    | member (e, Cons(x,xs))  =
        if e = x then true
        else member(e, xs);

fun add (e, Nil) = Cons(e, Nil)
    | add (e, Cons(x,xs)) =  
        if member(e, Cons(x,xs)) then Cons(x,xs) 
        else Cons(e, Cons(x,xs));
  
fun remove (_, Nil) = Nil
    | remove (e, Cons(x,xs)) =       
        if x = e then xs
            else Cons(x, remove(e, xs));
            
fun union (Cons(x,xs), y) = union(xs, add (x, y))
    | union (Nil, y) = y;
    

fun cartesian (Nil, _) = Nil
    | cartesian (_, Nil) = Nil
    | cartesian (Cons(x,xs), Cons(y,ys)) = 
    Cons((x,y),union(cartesian(Cons(x,xs),ys), cartesian(xs,Cons(y,ys))));
(*this is probably a horrible way of doing this, but I spent too long thinking about it, and am just sticking with something that works*)

fun intersection (_, Nil) = Nil
    | intersection (Nil,_) = Nil
    | intersection (Cons(x,xs), y) = 
        if member(x,y) then Cons(x, intersection(xs, y))
        else intersection(xs, y);
        
fun subset (Nil,_) = true
    | subset (_, Nil) = false
    | subset (Cons(x,xs), y) = 
        if member(x, y) then subset(xs, y)
        else false;
        

fun characteristic_function (Nil) = (fn (_) => false)
    | characteristic_function (x) = (fn (n) => member(n,x));


fun for_all (_, Nil) = false
    | for_all (f, Cons(x,xs)) =
        if f(x) then for_all(f, xs)
        else false;
        
        
fun exists (_, Nil) = false
    | exists (f, Cons(x,xs)) =
        if f(x) then true
        else exists(f, xs);
        
        
fun filter(_, Nil) = Nil
    | filter (f, Cons(x,xs)) = 
        if f(x) then Cons(x, filter(f,xs))
        else filter(f,xs);
