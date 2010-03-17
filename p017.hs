main = print $ show $ sum $ map eng [1..1000]


eng :: Int -> Int
eng 0 = 0
eng 1 = 3
eng 2 = 3
eng 3 = 5
eng 4 = 4
eng 5 = 4
eng 6 = 3
eng 7 = 5
eng 8 = 5
eng 9 = 4
eng 10 = 3
eng 11 = 6
eng 12 = 6
eng 13 = 8
eng 14 = 8
eng 15 = 7
eng 16 = 7
eng 17 = 9
eng 18 = 8
eng 19 = 8
eng 20 = 6
eng 30 = 6
eng 40 = 5
eng 50 = 5
eng 60 = 5
eng 70 = 7
eng 80 = 6
eng 90 = 6
eng 1000 = 11

eng n
    | (mod n 100) == 0 = (eng (truncate ((fromIntegral n)/100))) + 7
    | n > 100 = (eng (truncate ((fromIntegral n)/100))) + 7 + 3 +  (eng (mod n 100)) 
    | otherwise = (eng (truncate ((fromIntegral n)/10)*10)) + (eng (mod n 10))
