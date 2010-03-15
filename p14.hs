import List

--main = print $ show $ last $ sort $ map length $ mcollatz [1..20000] []
main = print $ show $ mcollatz [1..20] []

mcollatz [] ms = []
mcollatz (n:ns) ms = do let cl = collatz n ms
                        cl:(mcollatz ns (ms++[cl]))

collatz :: Integer -> [[Integer]] -> [Integer] 
collatz n ms
    |n==1 = [1]
    |ms /= [] && n < toInteger (length ms) = ms !! (fromInteger (n-1))
    |even n = n:(collatz (truncate ((fromIntegral n)/(fromIntegral 2)))　ms)
    |otherwise = n:(collatz (3*n + 1) ms)