fun fib_loop(n,fib,prev) = 
    if n <= 1 then n 
    else fib + fib_loop(n-1,fib+prev, fib);

fun fib(n) = fib_loop(n,0,1);


