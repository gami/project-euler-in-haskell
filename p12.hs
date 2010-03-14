
main = print $ show $ tri $ toInteger $ length (takeWhile (< 500) (map (\x -> triDivCount x ps) [1..]))+1
       where ps = primes 2000000

triDivCount n ps = divisorCount (tri n) ps
--tri 1 = 1
tri :: Integer -> Integer
tri n = sum [1..n]

divisorCount :: Integer -> [Integer]-> Integer
divisorCount n ps =  divc $ primeFactors n ps

divc :: [Integer] -> Integer
divc [] = 1
divc (x:xs) 
    | elem x xs = (toInteger ((length (cnt x xs))+2)) * (toInteger (divc (ncnt x xs)))
    | otherwise = 2 * (divc xs)

cnt n xs = filter (== n) xs
ncnt n xs = filter (/= n) xs

--length $ divisor n

divisor n = [1]++(divisor2 n [2..(n-1)])++[n]

divisor2 n [] = []
divisor2 n (x:xs) 
    | (mod n x)==0 = x:divisor2 n xs
    | otherwise = divisor2 n xs

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