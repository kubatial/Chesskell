module DataStructs where
import Data.HashMap as HM

-- structure to hold piece types or 'Empty' if no piece 
data Piece = 
    Pw Integer | Pb Integer | Rw Integer Bool | Rb Integer Bool 
    | Nw Integer | Nb Integer | Bw Integer | Bb Integer
    | Qw | Qb | Kw Bool Bool | Kb Bool Bool | Empty
    deriving (Show, Eq)

-- structure to hold various Actions types
data  Action =
      M Integer Integer
    | Help
    | Resign
    | Exit
    deriving (Show)
    
-- structure to hold game state
data State =
      WhiteCheck
    | BlackCheck
    | WhiteCheckmate
    | BlackCheckmate
    | Stalemate
    | Nothing
    deriving (Show)

-- bord type
type Board = HM.Map Integer Piece