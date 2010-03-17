main = print $ show $ sumDigits $ factorial 100

factorial 1 = 1
factorial n = n * (factorial (n-1))

sumDigits::Integer->Integer
sumDigits n = sum $ sumDigitsStr (show n)

sumDigitsStr::String->[Integer]
sumDigitsStr [] = []
sumDigitsStr (s:xs) = [read [s]::Integer] ++ (sumDigitsStr xs)
