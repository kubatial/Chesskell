module ProcessMove where
import DataStructs as DS
import Data.HashMap as HM

processMove :: DS.Board -> DS.Action -> (DS.Board, DS.State)
processMove board move@(DS.M s d) =
  (wipePiece (HM.insert d (extractPiece (HM.lookup s board)) board) s, 
            checkState board move)
        
{- Takes a board and target position (Integer) and returns a new
 - board with the target position wiped Empty 
 -}
wipePiece :: DS.Board -> Integer -> DS.Board
wipePiece board loc = (HM.insert loc DS.Empty board)


{- Takes a Maybe Piece and returns the associated Piece -}
extractPiece :: Maybe Piece -> Piece
extractPiece (Just p) = p
extractpiece Prelude.Nothing = DS.Empty


{- Takes a board and the latest move and returns the new game
 - state (e.g. BlackCheckMate, WhiteCheck, Nothing, etc.) 
 -}
checkState :: DS.Board -> DS.Action -> DS.State
checkState _ _ = DS.Nothing