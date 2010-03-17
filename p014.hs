import List

main = print $ show $ snd $ last $ sort $ mcollatz [1..1000000]
--main = print $ show $ last $ mcollatz [1..5000] []

mcollatz :: [Integer] -> [(Integer,Integer)]
mcollatz [] = []
mcollatz (n:ns) = (toInteger (length ( collatz n)),n):(mcollatz ns)
                        
--fcollatz [] ms = []
--fcollatz (n:ns) ms = do let cl = collatz n ms
--                        cl:(fcollatz (filter (\x -> not (elem x cl)) ns) ms)
                                                
collatz :: Integer -> [Integer] 
collatz n
    |n==1 = [1]
--    |n < ((last ms) !! 0) = ms !! (fromInteger (n-1))
    |even n = n:(collatz (truncate ((fromIntegral n)/(fromIntegral 2))))
    |otherwise = n:(collatz (3*n + 1))