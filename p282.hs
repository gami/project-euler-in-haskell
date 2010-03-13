main = print $ show $ sum $ map (\x -> ackerman2 x x) [1..6]

ackerman m n
    |m==0 = n+1
    |m > 0 && n == 0 = ackerman (m-1) 1
    |m > 0 && n > 0 = ackerman (m-1) (ackerman m (n-1))

ackerman2 m n
    |m==0 = n+1
    |m==1 = n+2
    |m==2 = 2*n+3
    |m==3 = 2^(n+3)-3
    |m > 3 && n == 0 = ackerman2 (m-1) 1
    |m > 3 && n > 0 = ackerman2 (m-1) (ackerman2 m (n-1))


