import Data.List

------------------------------------------------------------------------------------------
-----------------------------------------------------------------2.-Starting-out----------

doubleSmallNumber x = if x>100
			then x
			else x*2

multiply xs xt = [ x*y | x <- xs, y <- xt, x*y > 20]

removeNonUppercase xs = [ x | x <- xs, x `elem` ['A'..'Z'] ]

triangle = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

------------------------------------------------------------------------------------------
----------------------------------------------------------4.-Syntax-in-Functions----------

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs

------------------------------------------------------------------------------------------
--------------------------------------------------------------------5.-Recursion----------

maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "error"
maximum2 [x] = x
maximum2 (x:s) = max x (maximum2 s)

replicate2 :: (Integral a) => a -> a -> [a]
replicate2 0 _ = []
replicate2 x y = y:(replicate2 (x-1) y)

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:s) = t ++ [x] ++ u
	where t = quicksort [ y | y <- s, y < x ]
	      u = quicksort [ y | y <- s, y >= x ]  

------------------------------------------------------------------------------------------
-------------------------------------------------------6.-Higher-order-funcitons----------

foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f x [] = x
foldl2 f x (y:s) = foldl2 f (f x y) s

------------------------------------------------------------------------------------------
----------------------------------------------------------------------7.-Modules----------

------------------------------------------------------------------------------------------
-----------------------------------------8.-Making-our-own-Types and Typeclasses----------

data Shape = Circuit Float | Rectangle Float Float deriving (Show)

surface :: Shape -> Float
surface (Circuit r) = r ^ 2 * pi
surface (Rectangle a b) = a * b

data Vehicle = Vehicle { kind :: String
    , marque :: String
    , color :: String
    } deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Enum, Show)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

data TrafficLight = Green | Yellow | Red

instance Show TrafficLight where
    show Green = "Green light"
    show Yellow = "Yellow light"
    show Red = "Red light"

