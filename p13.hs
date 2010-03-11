import Data.List

main = do cs <- readFile "p13.txt"
          print $ show (read (take 10 $ show $ sums $ lines cs)::Integer)

sums [] = 0
sums (c:cs) = (read c::Integer) + (sums cs)