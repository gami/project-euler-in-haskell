main = print $ show $ sum $ primes 2000000

primes n = sieve (2:[3,5..n])

sieve [] = []
sieve (p:xs)
    | p^2 <= (last xs) = p:(sieve [x| x <- xs, x `mod` p /= 0])
    | otherwise = p:xs