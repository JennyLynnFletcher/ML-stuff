fun tails ([]) = [] 
    | tails (x::xs) = (x::xs)::tails(xs);
