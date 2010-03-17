main = print $ show $ (sum[1..100]) ^ 2 - sumOfSquares [1..100]
    
sumOfSquares [] = 0
sumOfSquares (x:xs) = (x^2) + sumOfSquares xs
