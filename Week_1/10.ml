fun for_loop (state_in, keep_going, loop_body) =
    let 
        fun do_loop state_in =
            if not (keep_going state_in) then
                state_in
            else
                do_loop (loop_body state_in)
    in
        do_loop state_in
    end; 

fun return_last(x, y, z) = z;
    
fun fib(n) = 
    if n = 0 then 0
    else return_last(for_loop((0,1,0), fn(i, next, fib) => i < n, fn(n, next, fib) => (n+1, next + fib, next)));
