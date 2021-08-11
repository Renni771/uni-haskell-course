module TicTacToe where 

-- TicTacToe model

data Token = X | O
    deriving (Show, Eq)

type Row = (Token, Token, Token)
data RowPosition = L | M | R -- Left, middle, right

type Board = (    Row -- Top 
                , Row -- Middle
                , Row -- Bottom
             )

{-
A board looks like this: 

 Top    =   (Token, Token, Token)
 Middle =   (Token, Token, Token)
 Bottom =   (Token, Token, Token)
-}

data GameState = Ongoing | Won | Invalid
    deriving (Show, Eq)


-- Define functions that determine whether a game is in progress, won, or invalid.  If a game iswon, then by whom?2

-- | Determine what state the game 
gameState :: Board -> Token
gameState = undefined 

wonRow :: Token -> Board -> Bool
wonRow t (top, mid, bot) = top == playerRow || mid == playerRow || bot == playerRow
    where playerRow = (t, t, t)

wonColumn :: Token -> Board -> Bool 
wonColumn t (top, mid, bot) = leftCol == playerCol || midCol == playerCol || rightCol == playerCol
    where
        playerCol = (t, t, t)
        leftCol = (getElem L top, getElem L mid, getElem L bot) 
        midCol = (getElem M top, getElem M mid, getElem M bot)
        rightCol = (getElem R top, getElem R mid, getElem R bot)

wonDiagonal :: Token -> Board -> Bool 
wonDiagonal t (top, mid, bot) = topLeftToBottomRightDiag || topRightToBottomLeftDiag
    where
        topLeftToBottomRightDiag = getElem L top == t && middleToken == t && getElem R bot == t
        topRightToBottomLeftDiag = getElem R top == t && middleToken == t && getElem L bot == t
        middleToken = getElem M mid

getElem :: RowPosition -> Row -> Token
getElem p (a, b, c) =
    case p of
        L -> a
        M -> b
        R -> c

-- Test Data
t = (O, X, X)
m = (X, X, O)
b = (X, O, X)
board = (t, m, b)