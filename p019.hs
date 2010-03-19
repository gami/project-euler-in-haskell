--(y,m,d,i)
main = print $ show $ length $ filter (\(a,b,c,d) -> (1900 < a) && (c == 1) && (mod d 7)==0) $ days (1900,1,1,1) 2001

days (y,m,d,i) e
    |y == e = []
    |d == 29 && m == 2 && not (leap y) = days (y,    (m+1), 1, i) e
    |d == 30 && m == 2                 = days (y,    (m+1), 1, i) e    
    |d == 31 && (elem m [4,6,9,11])    = days (y,    (m+1), 1, i) e
    |d == 32                           = days (y,    (m+1), 1, i) e
    |m == 13                           = days ((y+1),    1, 1, i) e
    |otherwise                         = (y,m,d,i):(days (y,m,(d+1),(i+1)) e)
    
leap y
    | (mod y 400) == 0 = True
    | (mod y 100) == 0 = False 
    | (mod y 4)   == 0 = True
    | otherwise        = False
