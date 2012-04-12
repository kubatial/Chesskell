{-# OPTIONS_GHC -Wall #-}

module DataStructs where
import Data.HashMap

-- structure to hold piece types or 'Empty' if no piece 
data Piece = 
    Pw | Pb | Rw Bool | Rb Bool | Nw | Nb | Bw | Bb | Qw | Qb | Kw Bool Bool | Kb Bool Bool | Empty
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
    | Nothing
    deriving (Show)

-- bord type
type Board = Map Integer Piece
