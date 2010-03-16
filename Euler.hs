module Euler(parseData, triangle, primes, primeFactors, divisor)

where
    parseData t = map (\x -> map (\s -> read s::Integer ) (words x)) $ lines t
    
    triangle :: [[Integer]] -> [[Integer]] -> [[Integer]]
    triangle [] ts = []
    triangle (x:xs) [] = [(x !! 0)]:(triangle xs [[(x !! 0)]])
    triangle (x:xs) ts = do let tr = triangle' x (last ts) 0
                            tr:(triangle xs (ts++[tr]))
    triangle' :: [Integer] -> [Integer] -> Integer -> [Integer]
    triangle' (y:ys) ts n
        | n == 0 = (y+(ts !! (fromIntegral n))):(triangle' ys ts (n+1))
        | toInteger(length(ts)) == n = [y+(ts !! (fromIntegral (n-1)))]
        | otherwise = (y+(max (ts !! (fromIntegral (n-1))) (ts !! (fromIntegral n) ))):(triangle' ys ts (n+1))
        
    primes :: Integer -> [Integer]
    primes m = sieve (2:[3,5..m])

    sieve (p:xs)
        | xs == [] = [p]
        | otherwise = p:(sieve [x | x <- xs, x `mod` p /=0])
    
    primeFactors :: Integer -> [Integer] -> [Integer]
    primeFactors n (p:ps)
        | n < 2 = []
        | (p:ps) == [] = []
        | (mod n p) == 0 = p:(primeFactors (divide n p) (p:ps))
        | otherwise = primeFactors n ps
    divide :: Integer -> Integer -> Integer
    divide a 0 = error("zero divide")
    divide a b = truncate( fromIntegral(a) / fromIntegral(b) )
    
    divisor :: Integer -> [Integer]
    divisor n = [1]++(divisor2 n [2..(n-1)])++[n]
    divisor2 n [] = []
    divisor2 n (x:xs) 
        | (mod n x)==0 = x:divisor2 n xs
        | otherwise = divisor2 n xs