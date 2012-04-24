module GetAction where
import DataStructs as DS
import Data.Maybe
import Data.List as L
import Data.HashMap as HM

getAction :: DS.Board -> DS.Board2 -> DS.State -> IO DS.Action
getAction board board2 state = putStrLn "Enter Move: " >> getLine >>=  (parseUserMove board board2 state) 

--Converts the user input into an appropriate action if it's legal
parseUserMove :: DS.Board -> DS.Board2 -> DS.State -> String -> IO DS.Action
parseUserMove board board2 state move
  | (move == "Exit") || (move == "exit") = return DS.Exit
  | (move == "Resign") || (move == "resign") = return DS.Resign
  | (move == "Help") || (move == "help") = return DS.Help
  | (move == "O-O") && (isKingSideAllowed board board2 state) = (print (getLegalWhiteMoves board board2 state)) >> return DS.KingSideCastle 
  | (move == "O-O-O") && (isQueenSideAllowed board board2 state) = (print (getLegalWhiteMoves board board2 state)) >> return DS.QueenSideCastle
  | ((length move ) == 8) && (isAllowedEnPassant board board2 state ep) = (print (getLegalWhiteMoves board board2 state)) >> return ep  
  | ((length move ) == 7) && (isAllowedPromotion board board2 state p) = (print (getLegalWhiteMoves board board2 state)) >> return p
  | ((length move ) == 5) && (isAllowedMove board board2 state m) = print (getBlackKing board2) >> (print (getLegalWhiteMoves board board2 state)) >> return m
  | otherwise = putStrLn "Please Enter a Legal Move: " >> getLine >>= (parseUserMove board board2 state)
--  | (length move) /= 5 = putStrLn "Please Enter Your Input as From-To " >> getLine >>= parseUserMove
--  | otherwise = return (M (convertToInt (take 2 move)) (convertToInt (drop 3 move)))
    where
      ep = E (convertToInt (take 2 move)) (convertToInt (take 2 (drop 3 move)))
      p = P (convertToInt (take 2 move)) (convertToInt (take 2 (drop 3 move))) (convertToPiece (drop 6 move) state)
      m = M (convertToInt (take 2 move)) (convertToInt (drop 3 move))

-----------------------------------------------------------------------------------------------------------------------------------------------
--Checks if the player is allowed to castle kingside
isKingSideAllowed :: DS.Board -> DS.Board2 -> DS.State -> Bool
isKingSideAllowed _ _ DS.WhiteCheck = False
isKingSideAllowed _ _ DS.BlackCheck = False
isKingSideAllowed _ _ DS.WhiteCheckmate = False
isKingSideAllowed _ _ DS.BlackCheckmate = False
isKingSideAllowed _ _ DS.Stalemate = False
isKingSideAllowed board board2 state@(DS.NothingWhite)
  | isNothing (HM.lookup (Kw False False) board2) = False --King has moved or is in check
  | isNothing (HM.lookup (Rw 2 False) board2) = False -- Kingside rook has moved or is captured
  | ((fromJust (HM.lookup 62 board)) /= Empty) && ((fromJust (HM.lookup 63 board)) /= Empty) = False --There are other pieces in the way
  | (elem 62 (getLegalBlackToMoves board board2 DS.NothingBlack)) || (elem 63 (getLegalBlackToMoves board board2 DS.NothingBlack)) = False --The castling squares are attacked 
  | otherwise = True
isKingSideAllowed board board2 state@(DS.NothingBlack)
  | isNothing (HM.lookup (Kb False False) board2) = False --King has moved or is in check
  | isNothing (HM.lookup (Rb 2 False) board2) = False -- Kingside rook has moved or is captured
  | ((fromJust (HM.lookup 6 board)) /= Empty) && ((fromJust (HM.lookup 7 board)) /= Empty) = False --There are other pieces in the way
  | (elem 6 (getLegalWhiteToMoves board board2 DS.NothingWhite)) || (elem 7 (getLegalWhiteToMoves board board2 DS.NothingWhite)) = False --The castling squares are attacked 
  | otherwise = True

--Checks if the player is allwed to castle queenside
isQueenSideAllowed :: DS.Board -> DS.Board2 -> DS.State -> Bool
isQueenSideAllowed _ _ DS.WhiteCheck = False
isQueenSideAllowed _ _ DS.BlackCheck = False
isQueenSideAllowed _ _ DS.WhiteCheckmate = False
isQueenSideAllowed _ _ DS.BlackCheckmate = False
isQueenSideAllowed _ _ DS.Stalemate = False
isQueenSideAllowed board board2 DS.NothingWhite
  | isNothing (HM.lookup (Kw False False) board2) = False --King has moved or is in check
  | isNothing (HM.lookup (Rw 1 False) board2) = False -- Queenside rook has moved or is captured
  | ((fromJust (HM.lookup 58 board)) /= Empty) && ((fromJust (HM.lookup 59 board)) /= Empty) && ((fromJust (HM.lookup 60 board)) /= Empty) = False --There are other pieces in the way
  | (elem 58 (getLegalBlackToMoves board board2 DS.NothingBlack)) || (elem 59 (getLegalBlackToMoves board board2 DS.NothingBlack)) ||
  		(elem 60 (getLegalBlackToMoves board board2 DS.NothingBlack)) = False --The castling squares are attacked 
  | otherwise = True
isQueenSideAllowed board board2 DS.NothingBlack
  | isNothing (HM.lookup (Kb False False) board2) = False --King has moved or is in check
  | isNothing (HM.lookup (Rb 1 False) board2) = False -- Kingside rook has moved or is captured
  | ((fromJust (HM.lookup 2 board)) /= Empty) && ((fromJust (HM.lookup 3 board)) /= Empty) && ((fromJust (HM.lookup 4 board)) /= Empty) = False --There are other pieces in the way
  | (elem 2 (getLegalWhiteToMoves board board2 DS.NothingWhite)) || (elem 3 (getLegalWhiteToMoves board board2 DS.NothingWhite)) || 
  		(elem 4 (getLegalWhiteToMoves board board2 DS.NothingWhite)) = False --The castling squares are attacked 
  | otherwise = True

--Checks if en passant is allowed in this position
isAllowedEnPassant :: DS.Board -> DS.Board2 -> DS.State -> DS.Action -> Bool
isAllowedEnPassant _ _ DS.WhiteCheckmate _ = False
isAllowedEnPassant _ _ DS.BlackCheckmate _ = False
isAllowedEnPassant _ _ DS.Stalemate _ = False
isAllowedEnPassant _ _ DS.WhiteCheck _ = False
isAllowedEnPassant _ _ DS.BlackCheck _ = False
isAllowedEnPassant board board2 DS.NothingWhite (E i j)
  | i <= j = False --pawns cannot move backwards
  | ((i-j) /= 7) && ((i-j) /= 9) = False --Pawns cannot move that way
  | (((i-j) == 7) && pawns1) || (((i-j) == 9) && pawns2) = True
  | otherwise = False
    where
      pawns1 = ((fromJust (HM.lookup (i+1) board)) == (Pb 1)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pb 2)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pb 3)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pb 4)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pb 5)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pb 6)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pb 7)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pb 8))  
      pawns2 = ((fromJust (HM.lookup (i-1) board)) == (Pb 1)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pb 2)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pb 3)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pb 4)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pb 5)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pb 6)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pb 7)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pb 8)) 
isAllowedEnPassant board board2 DS.NothingBlack (E i j)
  | i >= j = False --pawns cannot move backwards
  | ((j-i) /= 7) && ((j-i) /= 9) = False --Pawns cannot move that way
  | (((j-i) == 7) && pawns2) || (((j-i) == 9) && pawns1) = True
  | otherwise = False
    where
      pawns1 = ((fromJust (HM.lookup (i+1) board)) == (Pw 1)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pw 2)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pw 3)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pw 4)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pw 5)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pw 6)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pw 7)) || 
    		((fromJust (HM.lookup (i+1) board)) == (Pw 8))  
      pawns2 = ((fromJust (HM.lookup (i-1) board)) == (Pw 1)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pw 2)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pw 3)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pw 4)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pw 5)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pw 6)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pw 7)) || 
    		((fromJust (HM.lookup (i-1) board)) == (Pw 8))  
--Checks if Promotion to the given piece is allowe
isAllowedPromotion :: DS.Board -> DS.Board2 -> DS.State -> DS.Action -> Bool
isAllowedPromotion _ _ DS.WhiteCheckmate _ = False
isAllowedPromotion _ _ DS.BlackCheckmate _ = False
isAllowedPromotion _ _ DS.Stalemate _ = False
isAllowedPromotion _ _ DS.WhiteCheck _ = False
isAllowedPromotion _ _ DS.BlackCheck _ = False
isAllowedPromotion board board2 state (P i j piece)
  | ((state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)) && piece1 = True
  | ((state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)) && piece2 = True
  | otherwise = False
    where
      piece1 = (piece == (Qw 1)) ||
      		(piece == (Qw 2)) ||
      		(piece == (Nw 1)) ||
      		(piece == (Nw 2)) ||
      		(piece == (Nw 3)) ||
      		(piece == (Rw 1 True)) ||
      		(piece == (Rw 1 False)) ||
      		(piece == (Rw 2 True)) ||
      		(piece == (Rw 2 False)) ||
      		(piece == (Rw 3 True)) ||
      		(piece == (Rw 3 False)) ||
      		(piece == (Bw 1)) ||
      		(piece == (Bw 2)) ||
      		(piece == (Bw 3))
      piece2 = (piece == (Qb 1)) ||
      		(piece == (Qb 2)) ||
      		(piece == (Nb 1)) ||
      		(piece == (Nb 2)) ||
      		(piece == (Nb 3)) ||
      		(piece == (Rb 1 True)) ||
      		(piece == (Rb 1 False)) ||
      		(piece == (Rb 2 True)) ||
      		(piece == (Rb 2 False)) ||
      		(piece == (Rb 3 True)) ||
      		(piece == (Rb 3 False)) ||
      		(piece == (Bb 1)) ||
      		(piece == (Bb 2)) ||
      		(piece == (Bb 3))
--Checks if a general move is legal for the player
isAllowedMove :: DS.Board -> DS.Board2 -> DS.State -> DS.Action -> Bool
--isAllowedMove board board2 state@(DS.NothingWhite) (M i j) = elem (M i j) (getLegalWhiteMoves board board2 state)
--isAllowedMove board board2 state@(DS.NothingBlack) (M i j) = elem (M i j) (getLegalBlackMoves board board2 state)
isAllowedMove board board2 state action
  | state1 = elem action (getLegalWhiteMoves board board2 state)
  | state2 = elem action (getLegalBlackMoves board board2 state)
  | otherwise = False
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant) || (state == DS.WhiteCheck)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant) || (state == DS.BlackCheck)

--Used in Promotion of a pawn
convertToPiece :: String -> DS.State -> DS.Piece
convertToPiece s state
  | whiteState && (s == "Q") = Qw 2
  | whiteState && (s == "R") = Rw 3 True
  | whiteState && (s == "N") = Nw 3
  | whiteState && (s == "B") = Bw 3
  | blackState && (s == "Q") = Qb 2
  | blackState && (s == "R") = Rb 3 True
  | blackState && (s == "N") = Nb 3
  | blackState && (s == "B") = Bb 3
  where
    whiteState = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
    blackState = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

--Converts an input from coordinate type given by the user to an Integer
convertToInt :: String -> Integer
convertToInt ('a':x) = 1 + (8-(read x :: Integer))*8
convertToInt ('b':x) = 2 + (8-(read x :: Integer))*8
convertToInt ('c':x) = 3 + (8-(read x :: Integer))*8
convertToInt ('d':x) = 4 + (8-(read x :: Integer))*8
convertToInt ('e':x) = 5 + (8-(read x :: Integer))*8
convertToInt ('f':x) = 6 + (8-(read x :: Integer))*8
convertToInt ('g':x) = 7 + (8-(read x :: Integer))*8
convertToInt ('h':x) = 8 + (8-(read x :: Integer))*8
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Given a board and it's inverse hashmap, it will return all the legal moves for the white player
getLegalWhiteMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
getLegalWhiteMoves b b2 s = (generateRookMoves b b2 s) ++ (generateKnightMoves b b2 s) ++ (generateBishopMoves b b2 s) ++
				(generatePawnMoves b b2 s) ++ (generateQueenMoves b b2 s) ++ (generateKingMoves b b2 s)

--Helper function for seeing if the black king is in check
getLegalWhiteToMoves :: DS.Board -> DS.Board2 -> DS.State -> [Integer]
getLegalWhiteToMoves b b2 s = L.map extractTo (getLegalWhiteMoves b b2 (DS.NothingWhite))

extractTo :: DS.Action -> Integer
extractTo (M i j) = j
extractTo (E i j) = j
extractTo (P i j _) = j

--Given a board and it's inverse hashmap, it will return all the legal moves for the black player
getLegalBlackMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
getLegalBlackMoves b b2 s = (generateRookMoves b b2 s) ++ (generateKnightMoves b b2 s) ++ (generateBishopMoves b b2 s) ++
				(generatePawnMoves b b2 s) ++ (generateQueenMoves b b2 s) ++ (generateKingMoves b b2 s)

--Helper function for seeing if the black king is in check
getLegalBlackToMoves :: DS.Board -> DS.Board2 -> DS.State -> [Integer]
getLegalBlackToMoves b b2 s = L.map extractTo (getLegalBlackMoves b b2 (DS.NothingBlack))

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Generates the Legal Moves for all the white or the black rooks, 
--depending on whose turn it is
generateRookMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
generateRookMoves _ _ DS.WhiteCheckmate = []
generateRookMoves _ _ DS.BlackCheckmate = []
generateRookMoves _ _ DS.Stalemate = []
generateRookMoves board board2 (DS.BlackCheck) = keepLegalBlack moves king
  where
    moves = generateRookMoves board board2 (DS.NothingBlack)
    king = getBlackKing board2
generateRookMoves board board2 (DS.WhiteCheck) = keepLegalWhite moves king
  where
    moves = generateRookMoves board board2 (DS.NothingWhite)
    king = getWhiteKing board2
generateRookMoves board board2 state
  | state1 = concat (L.map (singleRookMoves board board2 state) (getAllWhiteRooksLoc board2 1))
  | state2 = concat (L.map (singleRookMoves board board2 state) (getAllBlackRooksLoc board2 1))
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

singleRookMoves :: DS.Board -> DS.Board2 -> DS.State -> Integer -> [DS.Action]
singleRookMoves board board2 s i = keepFirstSame i ((getLeftRookMoves i board s) ++ (getRightRookMoves i board s) ++ (getTopRookMoves i board s) ++ (getBottomRookMoves i board s))

getLeftRookMoves :: Integer -> DS.Board -> DS.State -> [DS.Action]
getLeftRookMoves i board state
  | (i `mod` 8) == 1 = []
  | state1 && (isWhite board (i-1)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i-1)) = []
  | state1 && (isBlack board (i-1)) = [M i (i-1)]
  | state2 && (isWhite board (i-1)) = [M i (i-1)]
  | otherwise = (M i (i-1)) : (getLeftRookMoves (i-1) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

getRightRookMoves :: Integer -> DS.Board -> DS.State -> [DS.Action]
getRightRookMoves i board state
  | (i `mod` 8) == 0 = []
  | state1 && (isWhite board (i+1)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i+1)) = []
  | state1 && (isBlack board (i+1)) = [M i (i+1)]
  | state2 && (isWhite board (i+1)) = [M i (i+1)]
  | otherwise = (M i (i+1)) : (getRightRookMoves (i+1) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

getTopRookMoves :: Integer -> DS.Board -> DS.State -> [DS.Action]
getTopRookMoves i board state
  | (i >=1) && (i <= 8) = []
  | state1 && (isWhite board (i-8)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i-8)) = []
  | state1 && (isBlack board (i-8)) = [M i (i-8)]
  | state2 && (isWhite board (i-8)) = [M i (i-8)]
  | otherwise = (M i (i-8)) : (getTopRookMoves (i-8) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

getBottomRookMoves :: Integer -> DS.Board -> DS.State -> [DS.Action]
getBottomRookMoves i board state
  | (i >=57) && (i <= 64) = []
  | state1 && (isWhite board (i+8)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i+8)) = []
  | state1 && (isBlack board (i+8)) = [M i (i+8)]
  | state2 && (isWhite board (i+8)) = [M i (i+8)]
  | otherwise = (M i (i+8)) : (getBottomRookMoves (i+8) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)
-------------------------------------------------------------------------------------------------------------------------------------------------------------
--Generates the Legal Moves for all the white or the black knights, 
--depending on whose turn it is
generateKnightMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
generateKnightMoves _ _ DS.WhiteCheckmate = []
generateKnightMoves _ _ DS.BlackCheckmate = []
generateKnightMoves _ _ DS.Stalemate = []
generateKnightMoves board board2 (DS.BlackCheck) = keepLegalBlack moves king
  where
    moves = generateKnightMoves board board2 (DS.NothingBlack)
    king = getBlackKing board2
generateKnightMoves board board2 (DS.WhiteCheck) = keepLegalWhite moves king
  where
    moves = generateKnightMoves board board2 (DS.NothingWhite)
    king = getWhiteKing board2
generateKnightMoves board board2 state
  | state1 = concat (L.map (singleKnightMoves board board2 state) (getAllWhiteKnightsLoc board2 1))
  | state2 = concat (L.map (singleKnightMoves board board2 state) (getAllBlackKnightsLoc board2 1))
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

singleKnightMoves :: DS.Board -> DS.Board2 -> DS.State -> Integer -> [DS.Action]
singleKnightMoves board board2 state i 
  | ((i `mod` 8) >= 3) && ((i `mod` 8) <= 6) = L.map (toMove i) (deleteSameColor board state [i-15, i-17, i-6, i-10, i+10, i+6, i+17, i+15])
  | (i `mod` 8) < 3 = L.map (toMove i) (deleteSameColor board state [i-15, i-17, i-6, i+10, i+6, i+17, i+15])
  | otherwise = L.map (toMove i) (deleteSameColor board state [i-15, i-17, i-6, i-10, i+6, i+17, i+15])

deleteSameColor :: DS.Board -> DS.State -> [Integer] -> [Integer]
deleteSameColor board state (x:xs)
  | isNothing (HM.lookup x board) = deleteSameColor board state xs
  | state1 && (isWhite board x) = deleteSameColor board state xs
  | state2 && (isBlack board x) = deleteSameColor board state xs
  | otherwise = x : (deleteSameColor board state xs)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)
deleteSameColor _ _ [] = []
----------------------------------------------------------------------------------------------------------------------------------------------------------------
--Generates the Legal Moves for all the white or black bishops,
--depending on whose turn it is
generateBishopMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
generateBishopMoves _ _ DS.WhiteCheckmate = []
generateBishopMoves _ _ DS.BlackCheckmate = []
generateBishopMoves _ _ DS.Stalemate = []
generateBishopMoves board board2 (DS.BlackCheck) = keepLegalBlack moves king
  where
    moves = generateBishopMoves board board2 (DS.NothingBlack)
    king = getBlackKing board2
generateBishopMoves board board2 (DS.WhiteCheck) = keepLegalWhite moves king
  where
    moves = generateBishopMoves board board2 (DS.NothingWhite)
    king = getWhiteKing board2
generateBishopMoves board board2 state
  | state1 = concat (L.map (singleBishopMoves board board2 state) (getAllWhiteBishopsLoc board2 1))
  | state2 = concat (L.map (singleBishopMoves board board2 state) (getAllBlackBishopsLoc board2 1))
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

singleBishopMoves :: DS.Board -> DS.Board2 -> DS.State -> Integer -> [DS.Action]
singleBishopMoves board board2 state i = keepFirstSame i ((getULeft i board state) ++ (getURight i board state) ++ (getLLeft i board state) ++ (getLRight i board state))

keepFirstSame :: Integer -> [DS.Action] -> [DS.Action]
keepFirstSame y (x:xs) =  (DS.M y (extractSecondCoor x)) : (keepFirstSame y xs)
keepFirstSame x [] = []

extractSecondCoor :: DS.Action -> Integer
extractSecondCoor (M i j) = j
extractSecondCoor (P i j k) = j
extractSecondCoor (E i j) = j

getULeft :: Integer -> DS.Board -> DS.State -> [DS.Action]
getULeft i board state
  | ((i `mod` 8) == 1) || ((i >= 1) && (i <= 8)) = []
  | state1 && (isWhite board (i-9)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i-9)) = []
  | state1 && (isBlack board (i-9)) = [M i (i-9)]
  | state2 && (isWhite board (i-9)) = [M i (i-9)]
  | otherwise = (M i (i-9)) : (getULeft (i-9) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

getURight :: Integer -> DS.Board -> DS.State -> [DS.Action]
getURight i board state
  | ((i `mod` 8) == 0) || ((i >= 1) && (i <= 8)) = []
  | state1 && (isWhite board (i-7)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i-7)) = []
  | state1 && (isBlack board (i-7)) = [M i (i-7)]
  | state2 && (isWhite board (i-7)) = [M i (i-7)]
  | otherwise = (M i (i-7)) : (getURight (i-7) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

getLLeft :: Integer -> DS.Board -> DS.State -> [DS.Action]
getLLeft i board state
  | ((i `mod` 8) == 1) || ((i >= 57) && (i <= 64)) = []
  | state1 && (isWhite board (i+7)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i+7)) = []
  | state1 && (isBlack board (i+7)) = [M i (i+7)]
  | state2 && (isWhite board (i+7)) = [M i (i+7)]
  | otherwise = (M i (i+7)) : (getLLeft (i+7) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

getLRight :: Integer -> DS.Board -> DS.State -> [DS.Action]
getLRight i board state
  | ((i `mod` 8) == 0) || ((i >= 57) && (i <= 64)) = []
  | state1 && (isWhite board (i+9)) = [] --Own piece is blocking it's path
  | state2 && (isBlack board (i+9)) = []
  | state1 && (isBlack board (i+9)) = [M i (i+9)]
  | state2 && (isWhite board (i+9)) = [M i (i+9)]
  | otherwise = (M i (i+9)) : (getLRight (i+9) board state)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)
-------------------------------------------------------------------------------------------------------------------------------------------------
--Generates all the legal moves for all the white and black queens,
--depending on whose turn it is
generateQueenMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
generateQueenMoves _ _ DS.WhiteCheckmate = []
generateQueenMoves _ _ DS.BlackCheckmate = []
generateQueenMoves _ _ DS.Stalemate = []
generateQueenMoves board board2 (DS.BlackCheck) = keepLegalBlack moves king
  where
    moves = generateQueenMoves board board2 (DS.NothingBlack)
    king = getBlackKing board2
generateQueenMoves board board2 (DS.WhiteCheck) = keepLegalWhite moves king
  where
    moves = generateQueenMoves board board2 (DS.NothingWhite)
    king = getWhiteKing board2
generateQueenMoves board board2 state
  | state1 = concat (L.map (singleQueenMoves board board2 state) (getAllWhiteQueensLoc board2 1))
  | state2 = concat (L.map (singleQueenMoves board board2 state) (getAllBlackQueensLoc board2 1))
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

singleQueenMoves :: DS.Board -> DS.Board2 -> DS.State -> Integer -> [DS.Action]
singleQueenMoves board board2 state i = (singleRookMoves board board2 state i) ++ (singleBishopMoves board board2 state i)
------------------------------------------------------------------------------------------------------------------------------------------------
--Generates all the legal moves for either the white or black king,
--depending on whose turn it is
generateKingMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
generateKingMoves _ _ DS.WhiteCheckmate = []
generateKingMoves _ _ DS.BlackCheckmate = []
generateKingMoves _ _ DS.Stalemate = []
generateKingMoves board board2 (DS.BlackCheck) = keepLegalBlack moves king
  where
    moves = generateKingMoves board board2 (DS.NothingBlack)
    king = getBlackKing board2
generateKingMoves board board2 (DS.WhiteCheck) = keepLegalWhite moves king
  where
    moves = generateKingMoves board board2 (DS.NothingWhite)
    king = getWhiteKing board2
generateKingMoves board board2 state
  | state1 = singleKingMoves board board2 state (getWhiteKing board2)
  | state2 = singleKingMoves board board2 state (getBlackKing board2)
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

singleKingMoves :: DS.Board -> DS.Board2 -> DS.State -> Integer -> [DS.Action]
singleKingMoves board board2 state i = L.map (toMove i) (deleteSameColor board state [i-8, i-9, i-7, i+8, i+9, i+7, i-1, i+1])
-------------------------------------------------------------------------------------------------------------------------------------------------
--Generates all the legal moves for either the white or black pawns,
--depending on whose turn it is
generatePawnMoves :: DS.Board -> DS.Board2 -> DS.State -> [DS.Action]
generatePawnMoves _ _ DS.WhiteCheckmate = []
generatePawnMoves _ _ DS.BlackCheckmate = []
generatePawnMoves _ _ DS.Stalemate = []
generatePawnMoves board board2 (DS.BlackCheck) = keepLegalBlack moves king
  where
    moves = generatePawnMoves board board2 (DS.NothingBlack)
    king = getBlackKing board2
generatePawnMoves board board2 (DS.WhiteCheck) = keepLegalWhite moves king
  where
    moves = generatePawnMoves board board2 (DS.NothingWhite)
    king = getWhiteKing board2
generatePawnMoves board board2 state
  | state1 = concat (L.map (singlePawnMoves board board2 state) (getAllWhitePawnsLoc board2 1))
  | state2 = concat (L.map (singlePawnMoves board board2 state) (getAllBlackPawnsLoc board2 1))
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

singlePawnMoves :: DS.Board -> DS.Board2 -> DS.State -> Integer -> [DS.Action]
singlePawnMoves board board2 state i = (getForwardMove i board state) ++ (getDiagonalMove i board state)

getForwardMove :: Integer -> DS.Board -> DS.State -> [DS.Action]
getForwardMove i board state
  | state1 && (isWhite board i) && ((i >= 49) && (i <= 56)) && (isEmpty board (i-8)) && (isEmpty board (i-16)) = [M i (i-8), M i (i-16)]
  | state1 && (isWhite board i) && ((i >= 49) && (i <= 56)) && (isEmpty board (i-8)) = [M i (i-8)]
  | state2 && (isBlack board i) && ((i >= 9) && (i <= 16)) && (isEmpty board (i+8)) && (isEmpty board (i+16)) = [M i (i+8), M i (i+16)]
  | state2 && (isBlack board i) && ((i >= 9) && (i <= 16)) && (isEmpty board (i+8)) = [M i (i+8)]
  | state1 && (isWhite board i) && (isEmpty board (i-8)) && ((i >= 9) && (i <= 16)) = [P i (i-8) (Qw 2), P i (i-8) (Rw 3 True), P i (i-8) (Nw 3), P i (i-8) (Bw 3)]
  | state1 && (isWhite board i) && (isEmpty board (i-8)) = [M i (i-8)]
  | state2 && (isBlack board i) && (isEmpty board (i+8)) && ((i >= 49) && (i <= 56)) = [P i (i+8) (Qb 2), P i (i+8) (Rb 3 True), P i (i+8) (Nb 3), P i (i+8) (Bb 3)]
  | state2 && (isBlack board i) && (isEmpty board (i+8)) = [M i (i+8)]
  | otherwise = []
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

getDiagonalMove :: Integer -> DS.Board -> DS.State -> [DS.Action]
getDiagonalMove i board state 
  | state1 = L.map (toMove i) (deleteEmpty board (deleteSameColor board state [i-9, i-7]))
  | state2 = L.map (toMove i) (deleteEmpty board (deleteSameColor board state [i+9, i+7]))
    where
      state1 = (state == DS.NothingWhite) || (state == DS.NothingWhiteEnPassant)
      state2 = (state == DS.NothingBlack) || (state == DS.NothingBlackEnPassant)

deleteEmpty :: DS.Board -> [Integer] -> [Integer]
deleteEmpty board (x:xs)
  | isEmpty board x = deleteEmpty board xs
  | otherwise = x : (deleteEmpty board xs)
deleteEmpty _ [] = []
-------------------------------------------------------------------------------------------------------------------------------------------------
--Helper functions for the move generation functions

toMove :: Integer -> Integer -> DS.Action
toMove i j = M i j

isWhite :: DS.Board -> Integer -> Bool
isWhite board i 
  | white = True
  | otherwise = False
    where
      white = (piece == Pw 1) ||
      		(piece == Pw 2) ||
      		(piece == Pw 3) ||
      		(piece == Pw 4) ||
      		(piece == Pw 5) ||
      		(piece == Pw 6) ||
      		(piece == Pw 7) ||
      		(piece == Pw 8) ||
      		(piece == Rw 1 False) ||
      		(piece == Rw 1 True) ||
      		(piece == Rw 2 False) ||
      		(piece == Rw 2 True) ||
      		(piece == Rw 3 False) ||
      		(piece == Rw 3 True)  ||
      		(piece == Nw 1) ||
      		(piece == Nw 2) ||
      		(piece == Nw 3) ||
      		(piece == Bw 1) ||
      		(piece == Bw 2) ||
      		(piece == Bw 3) ||
      		(piece == Qw 2) ||
      		(piece == Qw 1) ||
      		(piece == Kw False False) ||
      		(piece == Kw False True) ||
      		(piece == Kw True False) ||
      		(piece == Kw True True) 
      piece = fromJust (HM.lookup i board)

isBlack :: DS.Board -> Integer -> Bool
isBlack board i 
  | black = True
  | otherwise = False
    where
      black = (piece == Pb 1) ||
      		(piece == Pb 2) ||
      		(piece == Pb 3) ||
      		(piece == Pb 4) ||
      		(piece == Pb 5) ||
      		(piece == Pb 6) ||
      		(piece == Pb 7) ||
      		(piece == Pb 8) ||
      		(piece == Rb 1 False) ||
      		(piece == Rb 1 True) ||
      		(piece == Rb 2 False) ||
      		(piece == Rb 2 True) ||
      		(piece == Rb 3 False) ||
      		(piece == Rb 3 True)  ||
      		(piece == Nb 1) ||
      		(piece == Nb 2) ||
      		(piece == Nb 3) ||
      		(piece == Bb 1) ||
      		(piece == Bb 2) ||
      		(piece == Bb 3) ||
      		(piece == Qb 2) ||
      		(piece == Qb 1) ||
      		(piece == Kb False False) ||
      		(piece == Kb False True) ||
      		(piece == Kb True False) ||
      		(piece == Kb True True) 
      piece = fromJust (HM.lookup i board)

isEmpty :: DS.Board -> Integer -> Bool
isEmpty board i = (fromJust (HM.lookup i board)) == Empty

keepLegalBlack :: [DS.Action] -> Integer -> [DS.Action]
keepLegalBlack x _ = x

keepLegalWhite :: [DS.Action] -> Integer -> [DS.Action]
keepLegalWhite x _ = x

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Returns the location of all the white rooks on the board
getAllWhiteRooksLoc :: DS.Board2 -> Integer -> [Integer]
getAllWhiteRooksLoc b2 i
  | isJust (HM.lookup (Rw i False) b2) = (fromJust (HM.lookup (Rw i False) b2)) : (getAllWhiteRooksLoc b2 (i+1))
  | isJust (HM.lookup (Rw i True) b2) = (fromJust (HM.lookup (Rw i True) b2)) : (getAllWhiteRooksLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the black rooks on the board
getAllBlackRooksLoc :: DS.Board2 -> Integer -> [Integer]
getAllBlackRooksLoc b2 i
  | isJust (HM.lookup (Rb i False) b2) = (fromJust (HM.lookup (Rb i False) b2)) : (getAllBlackRooksLoc b2 (i+1))
  | isJust (HM.lookup (Rb i True) b2) = (fromJust (HM.lookup (Rb i True) b2)) : (getAllBlackRooksLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the white knights on the board
getAllWhiteKnightsLoc :: DS.Board2 -> Integer -> [Integer]
getAllWhiteKnightsLoc b2 i
  | isJust (HM.lookup (Nw i) b2) = (fromJust (HM.lookup (Nw i) b2)) : (getAllWhiteKnightsLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the black knights on the board
getAllBlackKnightsLoc :: DS.Board2 -> Integer -> [Integer]
getAllBlackKnightsLoc b2 i
  | isJust (HM.lookup (Nb i) b2) = (fromJust (HM.lookup (Nb i) b2)) : (getAllBlackKnightsLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the white bishops on the board
getAllWhiteBishopsLoc :: DS.Board2 -> Integer -> [Integer]
getAllWhiteBishopsLoc b2 i
  | isJust (HM.lookup (Bw i) b2) = (fromJust (HM.lookup (Bw i) b2)) : (getAllWhiteBishopsLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the black bishoops on the board
getAllBlackBishopsLoc :: DS.Board2 -> Integer -> [Integer]
getAllBlackBishopsLoc b2 i
  | isJust (HM.lookup (Bb i) b2) = (fromJust (HM.lookup (Bb i) b2)) : (getAllBlackBishopsLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the white queens on the board
getAllWhiteQueensLoc :: DS.Board2 -> Integer -> [Integer]
getAllWhiteQueensLoc b2 i
  | isJust (HM.lookup (Qw i) b2) = (fromJust (HM.lookup (Qw i) b2)) : (getAllWhiteQueensLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the black queens on the board
getAllBlackQueensLoc :: DS.Board2 -> Integer -> [Integer]
getAllBlackQueensLoc b2 i
  | isJust (HM.lookup (Qb i) b2) = (fromJust (HM.lookup (Qb i) b2)) : (getAllBlackQueensLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the white pawns on the board
getAllWhitePawnsLoc :: DS.Board2 -> Integer -> [Integer]
getAllWhitePawnsLoc b2 i
  | isJust (HM.lookup (Pw i) b2) = (fromJust (HM.lookup (Pw i) b2)) : (getAllWhitePawnsLoc b2 (i+1))
  | i < 8 = (getAllWhitePawnsLoc b2 (i+1))
  | otherwise = []

--Returns the location of all the black pawns on the board
getAllBlackPawnsLoc :: DS.Board2 -> Integer -> [Integer]
getAllBlackPawnsLoc b2 i
  | isJust (HM.lookup (Pb i) b2) = (fromJust (HM.lookup (Pb i) b2)) : (getAllBlackPawnsLoc b2 (i+1))
  | i < 8 = (getAllBlackPawnsLoc b2 (i+1))
  | otherwise = []

--Returns the location of the white king
getWhiteKing :: DS.Board2 -> Integer
getWhiteKing b2
  | isJust (HM.lookup (Kw True True) b2) = fromJust (HM.lookup (Kw True True) b2)
  | isJust (HM.lookup (Kw True False) b2) = fromJust (HM.lookup (Kw True False) b2)
  | isJust (HM.lookup (Kw False True) b2) = fromJust (HM.lookup (Kw False True) b2)
  | isJust (HM.lookup (Kw False False) b2) = fromJust (HM.lookup (Kw False False) b2)
  | otherwise = 0  
 
--Returns the location of the black king
getBlackKing :: DS.Board2 -> Integer
getBlackKing b2
  | isJust (HM.lookup (Kb True True) b2) = fromJust (HM.lookup (Kb True True) b2)
  | isJust (HM.lookup (Kb True False) b2) = fromJust (HM.lookup (Kb True False) b2)
  | isJust (HM.lookup (Kb False True) b2) = fromJust (HM.lookup (Kb False True) b2)
  | isJust (HM.lookup (Kb False False) b2) = fromJust (HM.lookup (Kb False False) b2)
  | otherwise = 0
