data Vector2D =  Vector2D Double Double
    deriving (Eq)

instance Num Vector2D where
    (+) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    (*) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 * x2) (y1 * y2)
    abs (Vector2D x y) = Vector2D (if negative x then x * (-1) else x) (if negative y then y * (-1) else y)
        where negative n = n < 0

    signum (Vector2D x y) = Vector2D x y
    fromInteger n = Vector2D n' n'
        where
            n' = fromIntegral n :: Double

    negate (Vector2D x y) = Vector2D (-x)(-y)

instance Show Vector2D where
    show (Vector2D x y) = "Vector2D(x: " ++ show x ++ ", y: " ++ show y ++ ")"