{-# LANGUAGE GADTs #-}

{-data Term =
    I Integer
    | Add Term Term
    | Sub Term Term
    | Multi Term Term
    deriving (Eq, Show)

eval :: Term -> Integer
eval (I n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Multi a b) = eval a * eval b
-}


data Term a where
    Val :: (Num x) => x -> Term x
    Boolean :: Bool -> Term Bool
    Add :: (Num x) => Term x -> Term x -> Term x 
    Sub :: (Num x) => Term x -> Term x -> Term x 
    Multi :: (Num x) => Term x -> Term x -> Term x 
    Equal :: (Eq t) => Term t -> Term t -> Term Bool

eval :: Term a -> a
eval (Val n) = n
eval (Boolean b) = b
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Multi a b) = eval a * eval b
eval (Equal a b) = eval a == eval b
