module Polynome
    ( Polynome (..)
    , ppPolynome
    , eval
    ) where

import Data.List

newtype Polynome a = P [a] deriving (Show, Eq)

instance Num a => Num (Polynome a) where
    (P s) + (P t) = P $ zipWith' (+) s t
    (P s) * (P t) = P $ result [ (fst x * fst y,snd x + snd y) | x <- zip s [0..], y <- zip t [0..] ]
    negate (P s) = P $ map negate s
    abs (P s) = P s
    signum (P s) = P [1]
    fromInteger i = P [fromInteger i]

-- | Converts polynomial into string
--
-- Example:
--
-- >>> ppPolynome "x" (P [1,2,0,-4])
-- "1 + 2*x - 4*x^3"
ppPolynome :: String            -- ^ Argument
           -> Polynome Integer  -- ^ Polynomial
           -> String            -- ^ \"pretty\" polynomial
ppPolynome a (P s)
    | null s = func ++ " 0"
    | null $ dropWhile (== 0) s = func ++ " 0"
    | otherwise = func ++ (if take 2 poly == " +" then drop 2 poly else poly)
        where func = "f " ++ a ++ " ="
              poly = foldl1 (++) (zipWith (condition a) s [0..])

-- | Auxiliary function to convert polynomial into string
condition :: String     -- ^ Argument
          -> Integer    -- ^ Coefficient
          -> Integer    -- ^ Exponent
          -> String     -- ^ ``pretty'' polynomial
condition a x y
    | x == 0 = ""
    | y == 0 = sign x ++ (show.abs) x
    | y == 1 = sign x ++ a
    | x `elem` [-1,1] = sign x ++ a ++ "^" ++ show y
    | otherwise = sign x ++ (show.abs) x ++ "*" ++ a ++ "^" ++ show y
        where sign b = if b > 0 then " + " else " - "

-- | Evaluates functional value of the given polynomial and the real-value
--
-- Example:
--
-- >>> eval (P [1,2,0,-4]) 2
-- -27
eval :: Num a => Polynome a -- ^ Polynomial
              -> a          -- ^ Real-value
              -> a          -- ^ Result
eval (P []) _ = 0
eval (P (a:s)) x = a + x * eval (P s) x

-- | Modified version of zipWith -- length of the longer parameter == length of the result
--
-- Example:
--
-- >>> zipWith' (+) [1,2] [3,4,5,6]
-- [4,6,5,6]
zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ [] s = s
zipWith' _ s [] = s
zipWith' f (x:s) (y:t) = f x y : zipWith' f s t

-- | Converts the given polynomial represented as a list of pairs into a list of values
result :: (Num a, Ord b) => [(a,b)] -- ^ List of pairs
                         -> [a]     -- ^ List of values
result [] = []
result s = map (foldl (\x y -> x + fst y) 0) (groupBy (\x y -> snd x == snd y) (sortBy (\x y -> snd x `compare` snd y) s))
