fun mapt = fn f => (fn [] => [] | (x::xs) => f x::(mapt f xs));
