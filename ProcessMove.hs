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

{- Takes a Board, a desired Piece, and returns its location in the board as a key value into the Board -}
getBoardLocation :: DS.Board -> DS.Piece -> Integer 
getBoardLocation b p = recurseOnBoard b p 1 

recurseOnBoard :: DS.Board -> DS.Piece -> Integer -> Integer
recurseOnBoard b p x 
	| _ _ 65 				= -1
	| extractPiece . lookup x b == p	= x 
	| otherwise 				= recurseOnBoard b p (x+1) 


{- Takes a board and the latest move and returns the new game
 - state (e.g. BlackCheckMate, WhiteCheck, Nothing, etc.) 
 -}
checkState :: DS.Board -> DS.Action -> DS.State
checkState b a 
	| checkEqualLocation black_king_loc white_list == True	= DS.BlackCheck
	| checkEqualLocation white_king_loc black_list == True	= DS.WhiteCheck
	  
	where 
		black_list 		= getLegalBlackMoves b 1 []
		white_list 		= getLegalWhiteMoves b 1 [] 
		black_king_loc 		= getBoardLocation b DS.Kb
		white_king_loc		= getBoardLocation b Ds.Kw 

checkEqualLocation :: Integer -> [DS.Action] -> Bool
checkEqualLocation x (a:as)
	| a == DS.M _ x = True
	| otherwise 	= checkEqualLocation x as 
checkEqualLocation _ [] = False









  
