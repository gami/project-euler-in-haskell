import Euler

main = print $ show $ sum $ filter (>0) $ map (\x -> if (powAll (digits x) 5) == x then x else 0) [2 .. 500000]

powAll [] n = 0
powAll (x:xs) n = x^n + (powAll xs n)