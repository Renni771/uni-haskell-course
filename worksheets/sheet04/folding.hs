module Folding where

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc []     = acc
foldr' f acc (x:xs) = foldr' f (x `f` acc) xs

or' :: [Bool] -> Bool
or' = foldr' (==) True 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f (x:xs) = foldr' (\x xs -> if f x then x : xs else xs ) [] xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f xs = foldr'  (\y ys -> f y : ys) [] xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' f = foldr' (\x acc' -> acc' `f` x)
foldl' f = foldr' (flip f) -- flip swaps the arguments around

-- | removes consecutive duplicates from a list 
remdups' :: Eq  a => [a] -> [a]
remdups' (x:xs) =  reverse $ foldr' (\x acc -> if x == head acc then acc else x : acc) [x] xs
