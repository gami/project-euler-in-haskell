import List

main = print $ show $ length $ unique $ [a^b| a <-[2..100], b<-[2..100]]

unique ax = unique' $ sort ax

unique' [] = []
unique' (a:ax)
    | ax == []  = [a]
    | a == ax!!0   = unique' ax
    | otherwise = a:(unique' ax)