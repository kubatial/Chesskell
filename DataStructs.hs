module DataStructs where
import Data.HashMap as HM
import Data.Hashable

-- structure to hold piece types or 'Empty' if no piece 
data Piece = 
    Pw Integer | Pb Integer | Rw Integer Bool | Rb Integer Bool 
    | Nw Integer | Nb Integer | Bw Integer | Bb Integer
    | Qw Integer | Qb Integer | Kw Bool Bool | Kb Bool Bool | Empty
    deriving (Show, Eq, Ord)

instance Hashable Piece where
  hash (Pw i) = 100 + (fromIntegral i)
  hash (Rw i b) = 500 + (fromIntegral i)
  hash (Nw i) = 300 + (fromIntegral i)
  hash (Bw i) = 350 + (fromIntegral i)
  hash (Qw i) = 900 + (fromIntegral i)
  hash (Kw b c) = 10000
  hash (Pb i) = 1000 + (fromIntegral i)
  hash (Rb i b) = 5000 + (fromIntegral i)
  hash (Nb i) = 3000 + (fromIntegral i)
  hash (Bb i) = 3500 + (fromIntegral i)
  hash (Qb i) = 9000 + (fromIntegral i)
  hash (Kb b c) = 100000
  hash (Empty) = 0


-- structure to hold various Actions types
data  Action =
      M Integer Integer
    | P Integer Integer Piece --Added for Promotion
    | E Integer Integer --Added to take care of en passant
    | KingSideCastle
    | QueenSideCastle
    | Help
    | Resign
    | Exit
    deriving (Show, Eq)

-- structure to hold game state
data State =
      WhiteCheck
    | BlackCheck
    | WhiteCheckmate
    | BlackCheckmate
    | Stalemate
    | NothingWhite --White to move. Normal case
    | NothingWhiteEnPassant --White to move. Can do en passant
    | NothingBlack --Black to move. Normal case.
    | NothingBlackEnPassant --Black to move. Can do en passant
    deriving (Show, Eq)

-- bord type
type Board = HM.Map Integer Piece

--Extra Hashmap for Efficiency
type Board2 = HM.Map Piece Integer
