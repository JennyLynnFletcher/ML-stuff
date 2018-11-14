fun lastr ([], prev) = prev
    | lastr  (x::xs, prev) = lastr(xs, x);
    
fun last(x::xs) = lastr(x::xs, x);

(*This code has time complexity of O(n) as the code iterates over the list once*)

