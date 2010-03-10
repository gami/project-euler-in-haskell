main = print $ show $ until (divAll [1..20]) (+p) p
    where p = multi $ primes 20
--main = print $ show $ multi $ primes 20

divAll [] n = True
divAll (x:xs) n = (n `mod` x) == 0 && (divAll xs n)

primes m = sieve (2:[3,5..m])

multi [] = 1
multi (x:xs) = x * (multi xs)

sieve (p:xs)
    | xs == [] = [p]
    | otherwise = p:(sieve [x | x <- xs, x `mod` p /=0])