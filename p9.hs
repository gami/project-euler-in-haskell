main = print $ show $ multiTuple $ take 1 $ (combi arr arr arr)
    where arr = [1..1000]

combi [] bs cs = []
combi (a:as) bs cs = (combi2 a bs cs)++(combi as bs cs)

combi2 a [] cs = []
combi2 a (b:bs) cs
    | (a < b) && (a^2 + b^2 == (1000 - a -b)^2) = (combi3 a b cs)++(combi2 a bs cs)
    | otherwise = (combi2 a bs cs)

combi3 a b [] = []
combi3 a b (c:cs) 
    | (b < c) && (a + b + c == 1000) = [(a,b,c)] ++ (combi3 a b cs)
    | otherwise = (combi3 a b cs)

multiTuple :: [(Integer, Integer, Integer)] -> Integer
multiTuple [(a,b,c)] = a*b*c
