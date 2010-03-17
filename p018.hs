import List
import Euler

main = do r <- readFile "data/p018.txt" 
          print $ show $ last $ sort $ last $ triangle (parseData r) []
