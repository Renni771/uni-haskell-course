import Data.List
import Test.QuickCheck -- For testing

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
max3 a b c = maxi a (maxi b c)

prop_max3 a b c | a == b && b == c = max3 a b c == a && max3 a b c == b && max3 a b c == c
prop_max3 a b c | a > b && a > c = max3 a b c == a
prop_max3 a b c | b > a && b > c = max3 a b c == b
prop_max3 a b c | c > a && c > b = max3 a b c == c

-- 1.3
-- 1.4

-- Exercise 2
-- TODO:

-- Exercise 3
-- TODO: