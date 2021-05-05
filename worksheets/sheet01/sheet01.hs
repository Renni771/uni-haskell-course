import Data.List

-- import Test.QuickCheck -- For testing

-- Exercise 1

-- 1.1

-- | Find the maximum of two integers.
maxi :: Int -> Int -> Int
maxi a b
  | a == b = a
  | a /= b = if a > b then a else b

-- maxi tests
prop_maxi :: Int -> Int -> Bool
prop_maxi a b
  | a == b = maxi a b == a && maxi a b == b
  | a > b = maxi a b == a
  | a < b = maxi a b == b

-- | Find the minimum of two integers
mini :: Int -> Int -> Int
mini a b
  | a == b = a
  | a /= b = if a < b then a else b

-- mini tests
prop_mini :: Int -> Int -> Bool
prop_mini a b
  | a == b = mini a b == a && mini a b == b
  | a > b = mini a b == b
  | a < b = mini a b == a

-- 1.2

-- | Find the maximum of three integers
max3 :: Int -> Int -> Int -> Int
max3 a b c
  | a == b && b == c = a
  | a > b && b > c = maxi a (maxi b c)
  | b > a && a > c = maxi a (maxi b c)
  | c > b && b > a = maxi a (maxi b c)
  | a == b && b > a = maxi a (maxi b c)
  | a == c && b > a = maxi a (maxi b c)

-- max3 tests
prop_max3 :: Int -> Int -> Int -> Bool
prop_max3 a b c
  | a == b && b == c = max3 a b c == a && max3 a b c == b && max3 a b c == c
  | a > b && b > c = max3 a b c == a -- a is the biggest
  | b > a && a > c = max3 a b c == b -- b is the biggest
  | c > b && b > a = max3 a b c == c -- c is the biggest
  | a == b && b > a = max3 a b c == a -- c is the biggest
  | a == c && b > a = max3 a b c == a -- c is the biggest

-- 1.3
-- TODO:

-- Exercise 2
-- TODO:

-- Exercise 3
head' :: [a] -> a
head' (a : rest) = a

tail' :: [a] -> [a]
tail' [] = []
tail' (a : rest) = rest

init' :: [a] -> [a]
init' [a] = []
init' (a : as) = a : init' as

last' :: [a] -> a
last' [a] = a
last' (a : as) = last' as

length' :: [a] -> Int
length' [] = 0
length' [a] = 1
length' (a : as) = 1 + length' as

reverse' :: [x] -> [x]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

(~++) :: [a] -> [a] -> [a]
(~++) [] ys = ys
(~++) (x : xs) ys = x : xs ++ ys

iterate'' :: (x -> x) -> x -> [x]
iterate'' f x = x : iterate'' f (f x)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (a : as) = f a : map' f as

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (a : as) | f a = a : filter' f as

intersperse' :: Char -> String -> String
intersperse' ' ' "" = ""
intersperse' a (b : bs) = b : a : intersperse' a bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [a] = a
concat' (a : as) = a ++ concat' as

zipWith' :: (t -> t -> t) -> [t] -> [t] -> [t]
zipWith' f [] ys = ys
zipWith' f xs [] = xs
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

repeat' :: a -> [a]
repeat' a = a : repeat' a

and' :: [Bool] -> Bool
and' [] = True
and' [a] = a
and' (x : xs) = x && and' xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xs@(x : xs')
  | f x = dropWhile' f xs'
  | otherwise = xs

maximum' :: Ord a => [a] -> a
maximum' [a] = a
maximum' (a : as) = max a (maximum' as)
