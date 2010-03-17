import List

--main = print $ show $ filter isPaindromicTuple $ cross [100..999] [100..999]
main = print $ show $ last $ sort $ map  multiTuple $ filter isPaindromicTuple $ cross [100..999] [100..999]

multiTuple (a,b) = a*b
isPaindromicTuple (a,b) = isPaindromic (divByKeta (a*b))

isPaindromic ns
    | (length ns) < 6 = False
    | otherwise = ((ns !! 5) == (ns !! 0)) && (ns !! 4) == (ns !! 1) && (ns !! 3) == (ns !! 2) 
    
divByKeta :: Integer -> [Integer]
divByKeta n
    | n < 10 = [(n `mod` 10)]
    | otherwise = (divByKeta (truncate ((fromIntegral n)/(fromIntegral 10))))++[(n `mod` 10)]

cross [] bs = []
cross (a:as) bs = (filter (\x -> fst(x) <= snd(x)) (crossl a bs))++(cross as bs)

crossl n [] = []
crossl n (x:xs) = [(n,x)]++(crossl n xs)