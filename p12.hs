import Euler

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
