--A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation
--of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically,
-- we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
--
--012   021   102   120   201   210
--
--What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

main = print $ show $ last $ take 1000000 [(a2s [a,b,c,d,e,f,g,h,i,j])|
                                        a<-[0..9],
                                        b<-[0..9],
                                        c<-[0..9],
                                        d<-[0..9],
                                        e<-[0..9],
                                        f<-[0..9],
                                        g<-[0..9],
                                        h<-[0..9],
                                        i<-[0..9],
                                        j<-[0..9],
                                        unq([a,b,c,d,e,f,g,h,i,j])]

a2s [] = []
a2s (i:is) = (show i)++(a2s is)

unq [] = True
unq (a:as)
    |not (unq' a as) = False
    |otherwise = unq as
 
unq' a [] = True
unq' a (b:bs)
    | (a == b) = False
    | otherwise = unq' a bs

-- I solved, but it took 818s. I need improve this.
