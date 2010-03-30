import Euler

main = print $ show $ sum $ filter (\x -> x == sum (map factorial (digits x))) [3..3000000]
