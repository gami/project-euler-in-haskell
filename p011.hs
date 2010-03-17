import List

main = do cs <- readFile "data/p011.txt"
          let mx = map words $ lines cs
          let arr = (mx++(map (\x -> tmap x mx) [0..19]))++(nmap n1map [0..19] mx)++(nmap n2map [0..19] mx)++(nmap n3map [0..19] mx)++(nmap n4map [0..19] mx)
          print $ show $ maxa $ (map yoko4 arr)
          

tmap n sx
    | n < 0 = []
    | n >= (length sx) = []
    | otherwise = map (\x-> x !! n) sx

nmap f [] sx = []
nmap f (n:nx) sx = (f n 0 sx):(nmap f nx sx)

n1map n m sx
    | n < 0 = []
    | m < 0 = []
    | n >= (length sx) = []
    | m >= (length sx) = []
    | otherwise = ((sx !! n)!!m):(n1map (n+1) (m+1) sx)
    
n2map n m sx
    | n < 0 = []
    | m < 0 = []
    | n >= (length sx) = []
    | m >= (length sx) = []
    | otherwise = ((sx !! (19-n))!!m):(n2map (n+1) (m+1) sx)
    
n3map n m sx
    | n < 0 = []
    | m < 0 = []
    | n >= (length sx) = []
    | m >= (length sx) = []
    | otherwise = ((sx !! n)!!(19-m)):(n3map (n+1) (m+1) sx)
    
n4map n m sx
    | n < 0 = []
    | m < 0 = []
    | n >= (length sx) = []
    | m >= (length sx) = []
    | otherwise = ((sx !! (19-n))!!(19-m)):(n4map (n+1) (m+1) sx)
        
maxa a = last $ sort a 

lastSafe :: [Integer]->Integer
lastSafe [] = 0
lastSafe a = last a

yoko4 x = lastSafe $ sort $ map multi (str4 x)

str4 :: [String] -> [[String]]
str4 [] = []
str4 (s:ss)
    | length (s:ss) < 4 = []
    | otherwise = (take 4 (s:ss)):(str4 ss)
         
multi :: [String] -> Integer
multi [] = 1
multi (s:ss) = (read s::Integer) * (multi ss)

