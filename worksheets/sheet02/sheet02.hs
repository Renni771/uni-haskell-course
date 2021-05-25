-- Exercise 1. (Fib)
naiveFib :: Integer -> Integer
naiveFib 0 = 0
naiveFib 1 = 1
naiveFib n = naiveFib (n - 1) + naiveFib (n -2)

fastFib :: Int -> Integer
fastFib = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fastFib (n - 1) + fastFib (n - 2)

-- Exercise 2. (Undup)
undup :: Eq a => [a] -> [a]
undup [] = []
undup (x : xs) = filter dup xs
  where
    dup x = x `elem` xs

prop_undup :: [a] -> Bool
prop_undup = undefined

-- Exercise 3. (Smallest Factor)

-- Exercise 4. (Media Library)

-- Exercise 5. (Tic-Tac-Toe)