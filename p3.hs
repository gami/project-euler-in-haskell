-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

main = print $ show $ primeFactors 600851475143　$ primes　10000
--main = print $ show $ primes 10000

primeFactors :: Integer -> [Integer] -> [Integer]
primeFactors n (p:ps)
    | n < 2 = []
    | (p:ps) == [] = []
--    | ps == [] = []
    | (mod n p) == 0 = p:(primeFactors (divide n p) (p:ps))
    | otherwise = primeFactors n ps


--primes n = [2,3,5,7,11,13,17,19,23,29]

divide :: Integer -> Integer -> Integer
divide a 0 = error("zero divide")
divide a b = truncate( fromIntegral(a) / fromIntegral(b) )

primes m = sieve (2:[3,5..m])

sieve (p:xs)
    | xs == [] = [p]
    | otherwise = p:(sieve [x | x <- xs, x `mod` p /=0])

end x y = fromInteger y > sqrt(fromInteger x )
    
isPrime x []     = True
isPrime x (y:ys)　= (x `mod` y > 0) && (isPrime x ys)
    
dumpList :: Integer -> [Integer] -> IO() 
dumpList i []     = print("")
dumpList i (x:xs) = do print(show(i) ++ ":" ++ show(x))
                       dumpList (i+1) xs

isMultipleOf x y = (x `mod` y) == 0