import List
import Euler

main = do r <- readFile "data/p067.txt" 
          print $ show $ last $ sort $ last $ triangle (parseData r) []
