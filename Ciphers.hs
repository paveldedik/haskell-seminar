module Ciphers
( encryptCaesar
, decryptCaesar
, encryptSubstitution
, decryptSubstitution
, encryptViginere
, decryptViginere
, encryptBlock
, decryptBlock
)
where

---Pomocné-funkce-------------------------------------------------------------------------
------------------------------------------------------------------------------------------

modBound :: Int -> Int
modBound x = x `mod` (fromEnum (maxBound :: Char) + 1) 

split :: Int -> String -> [String]
split _ [] = []
split x s = (take x s) : split x (drop x s) 

key = [('a','d'),('b','e'),('d','b'),('c','a'),('e','c')]

isPermutation :: [(Char, Char)] -> Bool
isPermutation s = s == [ x | x <- s, y <- s, z <- s, snd x == fst y, fst x == snd z ]

projectl :: [(Char, Char)] -> Char -> Char
projectl s x = if (t == []) then x else (snd.head) t
    where t = [ y | y <- s, fst y == x ]

projectr :: [(Char, Char)] -> Char -> Char
projectr s x = if (t == []) then x else (fst.head) t
    where t = [ y | y <- s, snd y == x ]

---Šifry----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Ke kazdemu znaku vstupniho Stringu pricte dany posun modulo maxBound.

encryptCaesar :: Int -> String -> String
encryptCaesar x = map (\z -> (toEnum . modBound) (fromEnum z + x))

-- | Od kazdeho znaku vstupniho Stringu odecte dany posun modulo maxBound.

decryptCaesar :: Int -> String -> String
decryptCaesar x = map (\z -> (toEnum . modBound) (fromEnum z - x))

-- | Zasifruje vstupni string dle dodane permutace.

encryptSubstitution :: [(Char, Char)] -> String -> String
encryptSubstitution s t
    | length s == 0 = t
    | isPermutation s = map (projectl s) t
    | otherwise = error "Nejedna se o permutaci."

-- | Desifruje vstupni string dle dodane permutace.

decryptSubstitution :: [(Char, Char)] -> String -> String
decryptSubstitution s t
    | length s == 0 = t
    | isPermutation s = map (projectr s) t
    | otherwise = error "Nejedna se o permutaci."

-- | Ke kazdemu znaku vstupniho Stringu pricte prislusny znak klice modulo maxBound.

encryptViginere :: String -> String -> String
encryptViginere [] s = s
encryptViginere s t = zipWith (\y z -> (toEnum . modBound) (fromEnum y + fromEnum z)) (cycle s) t

-- | Ke kazdemu znaku vstupniho Stringu pricte prislusny znak klice modulo maxBound.

decryptViginere :: String -> String -> String
decryptViginere [] s = s
decryptViginere s t = zipWith (\y z -> (toEnum . modBound) (fromEnum z - fromEnum y)) (cycle s) t

-- | Kazdy blok dane delky (pripadne kratsi, jedna-li se o posledni blok) zasifruje dle prislusne funkce.

encryptBlock :: [String -> String] -> Int -> String -> String
encryptBlock s x t
    | (x < 1) || (length s == 0) = t
    | otherwise = concat $ zipWith id (cycle s) (split x t)

-- | Kazdy blok dane delky (pripadne kratsi, jedna-li se o posledni blok) desifruje dle prislusne funkce.

decryptBlock :: [String -> String] -> Int -> String -> String
decryptBlock = encryptBlock












