main = print $ show $ sum $ digits $ show (2^1000)

digits::String->[Integer]
digits [] = []
digits (s:xs) = [read [s]::Integer] ++ (digits xs)


