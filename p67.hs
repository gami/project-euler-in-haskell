import List
import Euler

main = do r <- readFile "p67.txt" 
          print $ show $ last $ sort $ last $ triangle (parseData r) []
