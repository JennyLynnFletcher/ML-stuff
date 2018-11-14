
fun cons x y = x::y;

fun perms [] = [[]]
| perms xs =
let fun perms1 ([],ys) = []
| perms1 (x::xs,ys) =
map (cons x) (perms (rev ys @ xs)) @
perms1 (xs,x::ys)
in  perms1 (xs,[])  end;
