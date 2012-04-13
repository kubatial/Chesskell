module ProcessMove where
import DataStructs as DS
import Data.HashMap as HM

processMove :: DS.Board -> DS.Action -> (DS.Board, DS.State)
processMove board (DS.M s d) =
  (wipePiece (HM.insert d (extractPiece (HM.lookup s board)) board) s, DS.Nothing)
        
        
wipePiece :: DS.Board -> Integer -> DS.Board
wipePiece board loc = (HM.insert loc DS.Empty board)


extractPiece :: Maybe Piece -> Piece
extractPiece (Just p) = p
extractpiece Prelude.Nothing = DS.Empty