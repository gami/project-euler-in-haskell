import List
import Char

--main = print $ show $ alp 'A'
main = do cs <- readFile "data/p022.txt"
          print $ show $ score (sort $ split ',' cs) 1

split :: Char -> String -> [String]
split d [] = []
split d (s:ss) 
    | s == ',' = split d ss
    | otherwise = do let (h, t) = break (== d) (s:ss)
                     h:(split d t)
                     

score :: [String] -> Integer -> Integer
score [] i = 0
score (n:ns) i = ((nscore n) * i) + (score ns (i+1)) 

nscore :: String -> Integer
nscore [] = 0
nscore (s:ss) = (alp s) + (nscore ss)

alp :: Char -> Integer
alp c
    | isAlpha c && isUpper c = toInteger (ord c) - 64
    | otherwise = 0