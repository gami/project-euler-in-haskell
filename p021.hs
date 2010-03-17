import Euler

--main = print $ show $ map (\x -> sum (takeWhile(/= x) (divisor x))) [1..300]
main = print $ show $ sum $ amicables 10000 $ map (\x -> sum (takeWhile(/= x) (divisor x))) [1..10000]


amicables :: Integer -> [Integer] -> [Integer]
amicables 1 ds = []
amicables n ds = do let d1 = g1 (n-1) ds
                    let d2 = g1 (d1-1) ds
                    if d2 == n && d1 /= d2
                        then d1:(amicables (n-1) ds)
                        else amicables (n-1) ds                        
g1 n ds
    | n < 1 = 0 
    | n > 10000 = 0
    | otherwise = ds !! (fromIntegral n)



