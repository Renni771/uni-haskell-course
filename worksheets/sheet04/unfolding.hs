module Unfolding
    ( unfoldr
    , map'
    , iterate'
    )
where

import Data.Maybe

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f seed =  case f seed of
                   Nothing -> []
                   Just (a, b) -> a : unfoldr f b

-- | Map using unfold
map' :: (a -> b) -> [a] -> [b]
map' f = unfoldr (\a -> if null a then Nothing else Just (f $ head a, tail a))

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfoldr (\x -> Just (x, f x))
