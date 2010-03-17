import Euler

main = print $ show $ until (divAll [1..20]) (+p) p
    where p = product $ primes 20

divAll [] n = True
divAll (x:xs) n = (n `mod` x) == 0 && (divAll xs n)
