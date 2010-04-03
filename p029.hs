import List
import Euler

main = print $ show $ length $ unique $ [a^b| a <-[2..100], b<-[2..100]]
