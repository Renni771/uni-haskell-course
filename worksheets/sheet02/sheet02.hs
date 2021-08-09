-- import Test.QuickCheck

-- Exercise 1. (Fib)
--   Define the function which, for any natural numbern, computes then-the Fibunacci number:
naiveFib :: Integer -> Integer
naiveFib 0 = 0
naiveFib 1 = 1
naiveFib n = naiveFib (n - 1) + naiveFib (n -2)

-- Define an optimized version
fastFib :: Int -> Integer
fastFib = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fastFib (n - 1) + fastFib (n - 2)

-- Exercise 2. (Undup)
-- Define and test a function undup, which removes duplicated elements in a list.
undup :: Eq a => [a] -> [a]
undup [] = []
undup (x : xs)
  | x `elem` xs = x : filter (/= x) (undup xs)
  | otherwise = x : undup xs

-- Test undup
prop_undup :: Eq a => [a] -> Bool
prop_undup a
  | null a = null (undup a)
  | otherwise = a /= undup a -- TODO: Fix this test

-- Exercise 3. (Smallest Factor)
-- A natural number k ≥ 2 is factor of a number n if there exists m such that n = k · m. Define a
-- function that computes the smallest factor of a given Int.
-- Try to solve the tasks in two different ways and test them against each other.

-- Exercise 4. (Media Library)

-- Exercise 5. (Tic-Tac-Toe)