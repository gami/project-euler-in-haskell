main = print $ show $ combi 40 20

--combi :: Integer -> Integer -> Integer
combi a b = truncate(fromIntegral(frac a b)/fromIntegral(frac b b))
    
frac a b
    | a <= 1 = 1
    | b <  1 = 1
    | otherwise = a * (frac (a-1) (b-1))