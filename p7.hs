main = print $ show $ last $ take 10001 $ primes 

primes = sieve (2:[3,5..])

sieve (p:xs)
    | xs == [] = [p]
    | otherwise = p:(sieve [x | x <- xs, x `mod` p /=0])