main = print $ show $ sum $ map sum $ map diagonals [1,3..1001]
diagonals 1 = [1]
diagonals n = diagonals' n 3

diagonals' n (-1) = []
diagonals' n i = ((n^2)-((n-1)*i)):(diagonals' n (i-1))

