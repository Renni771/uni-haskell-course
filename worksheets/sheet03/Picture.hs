module Picture where

-- =======================
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

instance Semigroup Vector2D where
  (<>) = (+)

instance Monoid Vector2D where
  mempty = Vector2D 0 0
  mappend a b = a <> b

instance Show Vector2D where
    show (Vector2D x y) = "(x: " ++ show x ++ ", y: " ++ show y ++ ")"
-- =======================

-- | A type to describe lines, rectangles and circles.
data Picture = Line Vector2D Vector2D 
    | Rectangle Vector2D Vector2D Vector2D Vector2D 
    | Circle Vector2D Vector2D 

instance Show Picture where
    show (Line v1 v2) = "Line - start: " ++ show v1 ++ ", end: " ++ show v2
    show (Rectangle v1 v2 v3 v4) 
        = "Rectangle: (topLeft: " ++ show v1 ++ ", topRight: " ++ show v2 ++ ", bottomLeft: " ++ show v3 ++ ", bottomRight: " ++ show v4 ++ ")"
    show (Circle (Vector2D x1 y1) (Vector2D x2 y2)) = "Circle: (center: " ++ show (Vector2D  x1 y1) ++ ", radius" ++ show () ++ ")"

line = Line (Vector2D 1 1) (Vector2D 4 4)
rect = Rectangle (Vector2D 1 1) (Vector2D 1 4) (Vector2D 1 6) (Vector2D 1 6)
circle = Circle (Vector2D 1 1) (Vector2D 4 4)


buildLine :: Vector2D -> Vector2D -> Picture
buildLine = Line

buildCircle :: Vector2D -> Vector2D -> Picture
buildCircle  = Circle

buildRect :: Vector2D -> Vector2D -> Vector2D -> Vector2D -> Picture
buildRect (Vector2D x1 y1) (Vector2D x2 y2) (Vector2D x3 y3) (Vector2D x4 y4)
    | y1 == y2 && y3 == y4 = Rectangle (Vector2D 1 1) (Vector2D 1 4) (Vector2D 1 6) (Vector2D 1 6)
    | otherwise = Rectangle zero zero zero zero
    where zero = Vector2D  0 0
