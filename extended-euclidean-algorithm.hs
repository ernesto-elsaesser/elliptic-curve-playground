logLine is = putStrLn (foldl (\str i -> str ++ show i ++ " | ") "| " is)

logNext r0 x0 y0 r1 x1 y1 = let 
    r2 = mod r0 r1
    q = div r0 r1
    x2 = x0 - q * x1
    y2 = y0 - q * y1
    log = logLine [r1,q,x1,y1]
    in 
        if r2 == 0
        then log
        else log >> logNext r1 x1 y1 r2 x2 y2

eea a b = logLine [a,0,1,0] >> logNext a 1 0 b 0 1