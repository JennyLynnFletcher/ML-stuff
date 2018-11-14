(*
fun sumr (n) = 
    if n = 0 then 0
    else n + sumr(n-1);
    
fun sumi_loop (n, total) =
    if n = 0 then total
    else sumi_loop(n-1, total + n);
    
fun sumi (n) = sumi_loop(n, 0);
*)

fun sumr (0) = 0
    | sumr(n) = n + sumr(n-1);
    
fun sumi_loop (0, total) = total 
    | sumi_loop (n, total) = sumi_loop(n-1, total + n);
    
fun sumi (n) = sumi_loop(n, 0);

(*sumr has time complexity of O(n) and space complexity of O(n)
  sumi has time complexity of O(n) still, but a space complexity of O(1)*)
