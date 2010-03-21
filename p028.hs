--   2        4             6             8               10
--1, 3,5,7,9, 13,17,21,25,  31,37,43,49  57, 65, 73, 81, 91,101,111,121

--1,3,5,7,9,11

main = print $ show $ sum $ map sum $ map diagonals [1,3..1001]
diagonals 1 = [1]
diagonals n = diagonals' n 3

diagonals' n (-1) = []
diagonals' n i = ((n^2)-((n-1)*i)):(diagonals' n (i-1))

