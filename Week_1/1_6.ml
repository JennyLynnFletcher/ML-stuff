fun ex16(n) =
    if n = 0 then 0.5*(1.0 + Math.sqrt 5.0)
    else 1.0/(ex16(n-1) - 1.0);
