import Data.List

-- import Test.QuickCheck -- For testing

-- Exercise 1

-- 1.1

-- | Find the maximum of two integers.
maxi :: Int -> Int -> Int
maxi a b | a == b = a
maxi a b | a /= b = if a > b then a else b

-- maxi tests
prop_maxi a b | a == b = maxi a b == a && maxi a b == b
prop_maxi a b | a > b = maxi a b == a
prop_maxi a b | a < b = maxi a b == b

-- | Find the minimum of two integers
mini :: Int -> Int -> Int
mini a b | a == b = a
mini a b | a /= b = if a < b then a else b

-- mini tests
prop_mini a b | a == b = mini a b == a && mini a b == b
prop_mini a b | a > b = mini a b == b
prop_mini a b | a < b = mini a b == a

-- 1.2

-- | Find the maximum of three integers
max3 :: Int -> Int -> Int -> Int
max3 a b c | a == b && b == c = a
max3 a b c | a > b && b > c = maxi a (maxi b c)
max3 a b c | b > a && a > c = maxi a (maxi b c)
max3 a b c | c > b && b > a = maxi a (maxi b c)
max3 a b c | a == b && b > a = maxi a (maxi b c)
max3 a b c | a == c && b > a = maxi a (maxi b c)

-- max3 tests
prop_max3 :: Int -> Int -> Int -> Bool
prop_max3 a b c | a == b && b == c = max3 a b c == a && max3 a b c == b && max3 a b c == c
prop_max3 a b c | a > b && b > c = max3 a b c == a -- a is the biggest
prop_max3 a b c | b > a && a > c = max3 a b c == b -- b is the biggest
prop_max3 a b c | c > b && b > a = max3 a b c == c -- c is the biggest
prop_max3 a b c | a == b && b > a = max3 a b c == a -- c is the biggest
prop_max3 a b c | a == c && b > a = max3 a b c == a -- c is the biggest
-- 1.3
-- 1.4

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

-- (++) = undefined

iterate' = undefined

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (a : as) = f a : map' f as

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (a : as) | f a = a : filter' f as

intersperse' :: Char -> String -> String
intersperse' ' ' "" = ""
intersperse' a (b : bs) = b : a : intersperse' a bs

concat' = undefined

zipWith' = undefined

repeat' = undefined

and' = undefined

takeWhile' = undefined

dropWhile' = undefined

maximum' = undefined