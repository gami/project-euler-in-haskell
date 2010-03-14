-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

main = print $ show $ primeFactors 600851475143　$ primes　10000

primeFactors :: Integer -> [Integer] -> [Integer]
primeFactors n (p:ps)
    | n < 2 = []
    | (p:ps) == [] = []
    | (mod n p) == 0 = p:(primeFactors (divide n p) (p:ps))
    | otherwise = primeFactors n ps

divide :: Integer -> Integer -> Integer
divide a 0 = error("zero divide")
divide a b = truncate( fromIntegral(a) / fromIntegral(b) )

primes m = sieve (2:[3,5..m])

sieve (p:xs)
    | xs == [] = [p]
    | otherwise = p:(sieve [x | x <- xs, x `mod` p /=0])
    