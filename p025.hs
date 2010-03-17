import Euler

main = print $ show $ 1 + (length (takeWhile (\x -> (digitCount (show x)) < 1000) (map fib [1..])))