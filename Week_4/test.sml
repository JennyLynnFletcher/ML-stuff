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
                          if winnable_in n x then x::(bounded_winpath (n-1) (next_winnable_in n (next x))) else [];
