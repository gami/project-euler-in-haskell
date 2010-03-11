main = print $ show $ combi 39 20

combi :: Integer -> Integer -> Integer
combi a b
    | a > (a-b) = a * (combi (a-1) b)
    | otherwise = 1