import List
import Euler

main = do r <- readFile "p18.txt" 
          print $ show $ last $ sort $ last $ triangle (parseData r) []
