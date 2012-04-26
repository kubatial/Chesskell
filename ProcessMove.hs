module ProcessMove where
import DataStructs as DS
import Data.HashMap as HM
import GetAction as GA
import Data.List as L

processMove :: DS.Board -> DS.Board2 -> DS.State -> DS.Action -> (DS.Board, DS.Board2, DS.State)
processMove board board2 state move@(DS.M s d) = (updatedBoard, updatedBoard2, updatedState)
  where
    updatedBoard = wipePiece (HM.insert d (extractPiece (HM.lookup s board)) board) s
    updatedBoard2 = HM.delete (extractPiece (HM.lookup d board)) (HM.insert (extractPiece (HM.lookup s board)) d board2)
    updatedState = checkState updatedBoard updatedBoard2 state
processMove board board2 state move@(KingSideCastle)
    | state1 = (extractFirst whiteRookKingProcess, extractSecond whiteRookKingProcess, DS.NothingBlack)
    | state2 = (extractFirst blackRookKingProcess, extractSecond blackRookKingProcess, DS.NothingWhite)
      where
        state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
        state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)
        whiteKingProcess = processMove board board2 state (DS.M 61 63)
        blackKingProcess = processMove board board2 state (DS.M 5 7)
	whiteRookKingProcess = processMove (extractFirst whiteKingProcess) (extractSecond whiteKingProcess) (extractThird whiteKingProcess) (DS.M 64 62)
	blackRookKingProcess = processMove (extractFirst blackKingProcess) (extractSecond blackKingProcess) (extractThird blackKingProcess) (DS.M 8 6)
processMove board board2 state move@(QueenSideCastle)
    | state1 = (extractFirst whiteRookKingProcess, extractSecond whiteRookKingProcess, DS.NothingBlack)
    | state2 = (extractFirst blackRookKingProcess, extractSecond blackRookKingProcess, DS.NothingWhite)
      where
        state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
        state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)
        whiteKingProcess = processMove board board2 state (DS.M 61 59)
        blackKingProcess = processMove board board2 state (DS.M 5 3)
	whiteRookKingProcess = processMove (extractFirst whiteKingProcess) (extractSecond whiteKingProcess) (extractThird whiteKingProcess) (DS.M 57 60)
	blackRookKingProcess = processMove (extractFirst blackKingProcess) (extractSecond blackKingProcess) (extractThird blackKingProcess) (DS.M 1 4)
processMove board board2 state move@(P i j piece) = (updatedBoard, updatedBoard2, updatedState)
  where
    updatedBoard = wipePiece (HM.insert j piece board) i
    updatedBoard2 = HM.insert Empty i (HM.insert piece j board2)
    updatedState = checkState updatedBoard updatedBoard2 state
processMove board board2 state move@(E i j) = (updatedBoard, updatedBoard2, updatedState)
  where
    updatedBoard
      | (j < i) = wipePiece (wipePiece (HM.insert j (extractPiece (HM.lookup i board)) board) i) (j+8)
      | otherwise =  wipePiece (wipePiece (HM.insert j (extractPiece (HM.lookup i board)) board) i) (j-8)
    updatedBoard2  
      | (j < i) = HM.delete (extractPiece (HM.lookup (j+8) board)) ( HM.insert (extractPiece (HM.lookup i board)) j board2)
      | otherwise = HM.delete (extractPiece (HM.lookup (j-8) board)) ( HM.insert (extractPiece (HM.lookup i board)) j board2)
    updatedState = checkState updatedBoard updatedBoard2 state

extractFirst :: (a, b , c) -> a
extractFirst (x, y, z) = x

extractSecond :: (a, b ,c) -> b
extractSecond (x, y, z) = y

extractThird :: (a, b , c) -> c
extractThird (x, y, z) = z


wipePiece :: DS.Board -> Integer -> DS.Board
wipePiece board loc = (HM.insert loc DS.Empty board)


{- Takes a Maybe Piece and returns the associated Piece -}
extractPiece :: Maybe Piece -> Piece
extractPiece (Just p) = p
extractPiece (Nothing) = DS.Empty
--extractpiece Prelude.Nothing = DS.Empty


checkState :: DS.Board -> DS.Board2 -> DS.State -> DS.State
checkState board board2 s@(DS.WhiteCheck)
  | elem (GA.getBlackKing board2) (GA.getLegalWhiteToMoves board board2 s) = DS.BlackCheck
  | otherwise = DS.NothingBlack
checkState board board2 s@(DS.BlackCheck)
  | elem (GA.getWhiteKing board2) (GA.getLegalBlackToMoves board board2 s) = DS.WhiteCheck
  | otherwise = DS.NothingWhite
checkState board board2 s@(DS.NothingWhite)
  | (GA.getBlackKing board2) == 0 = DS.BlackCheckmate
  | (GA.getLegalBlackMoves board board2 DS.NothingBlack) == [] = DS.Stalemate
--  | isBlackCheckmate (L.map GA.extractTo (GA.generateKingMoves board board2 DS.NothingBlack)) (GA.getLegalWhiteToMoves board board2 s) board2 = DS.BlackCheckmate
  | elem (GA.getBlackKing board2) (GA.getLegalWhiteToMoves board board2 s) = DS.BlackCheck
  | otherwise = DS.NothingBlack
checkState board board2 s@(DS.NothingBlack)
  | (GA.getWhiteKing board2) == 0 = DS.WhiteCheckmate
  | (GA.getLegalWhiteMoves board board2 DS.NothingWhite) == [] = DS.Stalemate
--  | isWhiteCheckmate (L.map GA.extractTo (GA.generateKingMoves board board2 DS.NothingWhite)) (GA.getLegalBlackToMoves board board2 s) board2 = DS.WhiteCheckmate
  | elem (GA.getWhiteKing board2) (GA.getLegalBlackToMoves board board2 s) = DS.WhiteCheck
  | otherwise = DS.NothingWhite


isWhiteCheckmate :: [Integer] -> [Integer] -> DS.Board2 -> Bool
isWhiteCheckmate (x:xs) y b
  | elem x y = isWhiteCheckmate xs y b
  | otherwise = False
isWhiteCheckmate [] y b = elem (GA.getWhiteKing b) y

isBlackCheckmate :: [Integer] -> [Integer] -> DS.Board2 -> Bool
isBlackCheckmate (x:xs) y b
  | elem x y = isBlackCheckmate xs y b
  | otherwise = False
isBlackCheckmate [] y b = elem (GA.getBlackKing b) y 









  
