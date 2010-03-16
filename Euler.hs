module Euler(parseData, triangle)

where
    parseData t = map (\x -> map (\s -> read s::Integer ) (words x)) $ lines t
    
    triangle :: [[Integer]] -> [[Integer]] -> [[Integer]]
    triangle [] ts = []
    triangle (x:xs) [] = [(x !! 0)]:(triangle xs [[(x !! 0)]])
    triangle (x:xs) ts = do let tr = triangle' x (last ts) 0
                            tr:(triangle xs (ts++[tr]))
    triangle' :: [Integer] -> [Integer] -> Integer -> [Integer]
    triangle' (y:ys) ts n
        | n == 0 = (y+(ts !! (fromIntegral n))):(triangle' ys ts (n+1))
        | toInteger(length(ts)) == n = [y+(ts !! (fromIntegral (n-1)))]
        | otherwise = (y+(max (ts !! (fromIntegral (n-1))) (ts !! (fromIntegral n) ))):(triangle' ys ts (n+1))
    