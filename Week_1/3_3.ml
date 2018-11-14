(*
fun even_loop ([], flag, new_list) = new_list |
    even_loop (x::xs, flag, new_list) =
        if flag = 0 then even_loop(xs, 1, new_list)
        else even_loop(xs, 0, new_list@[x]);
    
fun even(x::xs) = even_loop(x::xs, 0, []);
*)

(*
fun evenr ([], flag) = [] |
    evenr (x::xs, false) = evenr(xs, true) |
    evenr (x::xs, true) = x::evenr(xs, false);
        
fun even (a) = evenr (a, false);
*)

fun even (xa::xb::xs) = xb::even(xs)
    | even _ = [];

(*The tail recursive version is less efficient due to calling the @ function which is a function of time complexity O(n) for every iteration through the list of n, giving a complexity of O(n^2), whereas the recursive version is only of time complexity O(n), as it only has to iterate through the list once*)
