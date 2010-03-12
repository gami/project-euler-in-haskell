main = print $ show $ length $ takeWhile (\x -> (digitCount (show x)) < 1000) (map fastfib [1..])
--main = print $ show $ takeWhile (\x -> (digitCount (show x)) < 3) [1..1000]
--main = print $ show $ (digitCount (show 1234) == 4) 

fastfib :: Integer -> Integer
fastfib n = fst (fibPair n)

fibPair :: Integer -> (Integer, Integer)
fibPair 1 = (1, 2)
fibPair n = fibStep(fibPair(n-1))

fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u+v)

digitCount::String->Integer
digitCount [] = 0
digitCount (s:xs) = 1+(digitCount xs)