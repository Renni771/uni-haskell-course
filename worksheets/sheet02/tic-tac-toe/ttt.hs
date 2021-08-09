module TicTacToe where 

data Token = X | O
    deriving (Show, Eq)

type Row = (Token, Token, Token)
newtype Board = Board (Row, Row, Row) -- Top Middle Bottom