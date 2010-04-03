import Euler
import List

main = print $ show $ sum $ filter (\x -> not (containInSortedArr x es)) [1..28123]
       where es = unique $ filter (<=28123) $ combi $ filter exceeds [1..28123]

exceeds n = n < (sum (trueDivisor n))

combi xs = [a+b| a<-xs, b<-xs, a<=b ]

containInSortedArr x [] = False
containInSortedArr x (a:as)
    | x > a = containInSortedArr x as
    | x == a = True
    | otherwise = False