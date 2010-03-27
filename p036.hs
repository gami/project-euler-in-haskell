import Euler

main = print $ show $ sum $ filterPaindromic2 $ filterPaindromic10 [1..999999]

filterPaindromic10 xs = filter (\x -> isPaindromic (digits x)) xs
filterPaindromic2  xs = filter (\x -> isPaindromic (digitsn x 2)) xs
