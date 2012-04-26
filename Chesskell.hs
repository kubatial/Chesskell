--The main file that runs the game

module Chesskell where

import Data.HashMap as HM
import DataStructs as DS
import GetAction as GA
import ProcessMove as PM
import Data.Tree
import Prelude as P
-----------------------------------------------------------------------------


-- Board intialization
initBoard::DS.Board
initBoard = 
  fromList ([(1,DS.Rb 1 False), (2,DS.Nb 1), (3,DS.Bb 1), (4,DS.Qb 1), 
            (5,DS.Kb False False), (6,DS.Bb 2), (7,DS.Nb 2),
            (8,DS.Rb 2 False), (9,DS.Pb 1), (10,DS.Pb 2), (11,DS.Pb 3), 
            (12,DS.Pb 4), (13,DS.Pb 5), (14,DS.Pb 6), (15,DS.Pb 7), 
            (16,DS.Pb 8)] ++ [(a,Empty) | a<-[17..48]] ++ 
           [(49,DS.Pw 1), (50,DS.Pw 2), (51,DS.Pw 3), (52,DS.Pw 4), 
            (53,DS.Pw 5), (54,DS.Pw 6), (55,DS.Pw 7), (56,DS.Pw 8), 
            (57,DS.Rw 1 False), (58,DS.Nw 1), (59,DS.Bw 1), (60,DS.Qw 1),
            (61,DS.Kw False False), (62,DS.Bw 2), (63,DS.Nw 2), (64,DS.Rw 2 False)])

--Reverse HashMap of board initialization
initBoard2 :: DS.Board2
initBoard2 = fromList ([(DS.Rb 1 False, 1), (DS.Nb 1, 2), (DS.Bb 1, 3), (DS.Qb 1, 4),
		(DS.Kb False False, 5), (DS.Bb 2, 6), (DS.Nb 2, 7),
		(DS.Rb 2 False, 8), (DS.Pb 1, 9), (DS.Pb 2, 10), (DS.Pb 3, 11),
		(DS.Pb 4, 12), (DS.Pb 5, 13), (DS.Pb 6, 14), (DS.Pb 7, 15),
		(DS.Pb 8, 16)] ++ [(Empty, a) | a<-[17..48]] ++ 
		[(DS.Pw 1, 49), (DS.Pw 2, 50), (DS.Pw 3, 51), (DS.Pw 4, 52),
		(DS.Pw 5, 53), (DS.Pw 6, 54), (DS.Pw 7, 55), (DS.Pw 8, 56),
		(DS.Rw 1 False, 57), (DS.Nw 1, 58), (DS.Bw 1, 59), (DS.Qw 1, 60),
		(DS.Kw False False, 61), (DS.Bw 2, 62), (DS.Nw 2, 63), (DS.Rw 2 False, 64)])

--General IO Parsing to Start Everything 
main :: IO ()
main = printRules >> getLine >>= parseInput


printRules :: IO ()
printRules = putStr (unlines ["\n\nWELCOME TO CHESSKELL!" , 
	"\nRules: " , 
	"You can choose between the following options" ,
	"1) Human vs. Human - Where 2 human players enter moves" ,
	"2) Human vs. Machine1 - Human plays against minimax machine" ,
	"When you enter a move you should type in the square you are" ,
	"moving a piece from to the square you are moving to, E.g e2-e4",
	"There are 3 kinds of special moves: Castling, Promotion and En Passant.",
	"You enter those moves as follows:\n",
	"   *O-O - Kingside Castling",
	"   *O-O-O - Queenside Castling",
	"   *Add -(PieceName) after you move a pawn to promotion, E.g e7-e8-Q",
	"   *Add -(ep) after you take a pawn en passant, E.g e5-d6-ep\n",
	"Finally, you have the following options: " ,
	"   *Exit - Will exit the current game" ,
	"   *Resign - you resign the current game" , 
	"   *Help - Print this list of instructions again" ,
	"Enter your choice of game or type 'Exit':"])

printHelp :: IO ()
printHelp = putStrLn (unlines [
	"When you enter a move you should type in the square you are" ,
	"moving a piece from to the square you are moving to, E.g e2-e4",
	"There are 3 kinds of special moves: Castling, Promotion and En Passant.",
	"You enter those moves as follows:\n",
	"   *O-O - Kingside Castling",
	"   *O-O-O - Queenside Castling",
	"   *Add -(PieceName) after you move a pawn to promotion, E.g e7-e8-Q",
	"   *Add -(ep) after you take a pawn en passant, E.g e5-d6-ep\n",
	"Finally, you have the following options: " ,
	"   *Exit - Will exit the current game" ,
	"   *Resign - you resign the current game" , 
	"   *Help - Print this list of instructions again" ,
	"Enter your choice of game or type 'Exit':"])

parseInput :: String -> IO ()
parseInput s
  | (s == "Exit") || (s == "exit")  = processAction 1 initBoard initBoard2 DS.NothingWhite DS.Exit
  | s == "1" = humanVsHuman (initBoard, initBoard2, DS.NothingWhite)
  | s == "2" = humanVsMachine1 (initBoard, initBoard2, DS.NothingWhite)
  | (s == "Help") || (s == "help")  = printRules >> getLine >>= parseInput
  | (s == "Resign") || (s == "resign") = processAction 1 initBoard initBoard2 DS.NothingWhite DS.Resign
  | otherwise = (putStrLn "Please enter a valid input: ") >> getLine 
  			>>= parseInput

----------------------------------------------------------------------
--Human vs Human Functionality
humanVsHuman :: (DS.Board, DS.Board2, DS.State) -> IO ()
humanVsHuman (b, b2, s) 
  | s == (DS.BlackCheckmate) = putStrLn "\nWhite wins. \n Thank you for playing Chesskell!\n"
  | s == (DS.WhiteCheckmate) = putStrLn "\nBlack wins. \n Thank you for playing Chesskell!\n"
  | s == (DS.BlackCheckmate) = putStrLn "\nThe game ended in a stalemate. \n Thank you for playing Chesskell!\n"
  | otherwise =  (printBoard 1 b) >> (GA.getAction b b2 s) 
                      >>= (processAction 1 b b2 s)

printBoard :: Integer -> Board -> IO ()
printBoard x b
  | x == 1 = putStr ("\n\n   " ++ (take 10 (repeat ' '))) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >> 
        putStr ((take 10 (repeat ' ')) ++ "8  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | x == 9 = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >> 
        putStr ((take 10 (repeat ' ')) ++ "7  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | x == 17 = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >>
        putStr ((take 10 (repeat ' ')) ++ "6  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | x == 25 = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >>
        putStr ((take 10 (repeat ' ')) ++ "5  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | x == 33 = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >>
        putStr ((take 10 (repeat ' ')) ++ "4  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | x == 41 = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >>
        putStr ((take 10 (repeat ' ')) ++ "3  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | x == 49 = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >>
        putStr ((take 10 (repeat ' ')) ++ "2  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | x == 57 = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >>
        putStr ((take 10 (repeat ' ')) ++ "1  ") >> 
        putStr ("|" ++ (pieceToString (b ! x))) >> (printBoard (x+1) b)
  | (x <= 64) && ((x `mod` 8) == 0) = 
        putStrLn ("|" ++ (pieceToString (b ! x)) ++ "|") >> 
        (printBoard (x+1) b)
  | (x <= 64) = putStr ("|" ++ (pieceToString (b ! x))) >> 
        (printBoard (x+1) b) 
  | otherwise = putStr ("   " ++ take 10 (repeat ' ')) >>
        putStr ((take 33 (repeat '-')) ++ "\n") >>
        putStrLn ((take 10 (repeat ' ')) ++ 
                "     a   b   c   d   e   f   g   h\n")

pieceToString :: Piece -> String
pieceToString (Pw _) = " P "
pieceToString (Pb _) = " p "
pieceToString (Rw _ _) = " R "
pieceToString (Rb _ _) = " r "
pieceToString (Nw _) = " N "
pieceToString (Nb _) = " n "
pieceToString (Bw _) = " B "
pieceToString (Bb _) = " b "
pieceToString (Qw _) = " Q "
pieceToString (Qb _) = " q "
pieceToString (Kw _ _) = " K "
pieceToString (Kb _ _) = " k "
pieceToString _ = "   "


processAction :: Integer -> DS.Board -> DS.Board2 -> DS.State -> DS.Action -> IO ()

processAction _ _ _ _ DS.Exit = 
    putStrLn "\nEXITING... \nThank you for playing Chesskell!\n"

processAction _ _ _ _ DS.Resign = 
    putStrLn "\nYou have resigned. \nThank you for playing Chesskell!\n" 

processAction _ _ _ DS.WhiteCheckmate _ = 
    putStrLn "\nBlack wins. \n Thank you for playing Chesskell!\n"
   
processAction _ _ _ DS.BlackCheckmate _ = 
    putStrLn "\nWhite wins. \n Thank you for playing Chesskell!\n"

processAction _ _ _ DS.Stalemate _ = 
    putStrLn "\nThe game ended in a Stalemate. \n Thank you for playing Chesskell!\n"

processAction i board board2 state DS.Help
  | i == 1 = printRules >> (humanVsHuman (board, board2, state) )
  | i == 2 = printRules >> (humanVsMachine1 (board, board2, state))

processAction i board board2 state move 
  | i == 1 = humanVsHuman (PM.processMove board board2 state move)  
  | (i==2) && (state == BlackCheckmate) = putStrLn "\nWhite wins. \n Thank you for playing Chesskell!\n"
  | (i==2) && (state == WhiteCheckmate) = putStrLn "\nBlack wins. \n Thank you for playing Chesskell!\n"
  | (i==2) && (state == Stalemate) = putStrLn "\nThe game ended in a stalemate. \n Thank you for playing Chesskell!\n"
  | i == 2 = humanVsMachine1 (minimax tree)
  | otherwise = humanVsHuman (PM.processMove board board2 state move)
    where
      m = PM.processMove board board2 state move
      tree = generateGameTree 2 m

-----------------------------------------------------------------------------
--Human vs. Machine1 Functionality
humanVsMachine1 :: (DS.Board, DS.Board2, DS.State) -> IO ()
humanVsMachine1 (b, b2, s) 
  | s == (DS.BlackCheckmate) = putStrLn "\nWhite wins. \n Thank you for playing Chesskell!\n"
  | s == (DS.WhiteCheckmate) = putStrLn "\nBlack wins. \n Thank you for playing Chesskell!\n"
  | s == (DS.Stalemate) = putStrLn "\nThe game ended in a stalemate. \n Thank you for playing Chesskell!\n"
  | otherwise = (printBoard 1 b)  >> (GA.getAction b b2 s) 
                      >>= (processAction 2 b b2 s)-- >>= generateGameTree >>= minimax


--First Integer just starts the eval. Second Integer is the current value.
evaluatePosition :: Integer -> Integer -> DS.Board -> Integer
evaluatePosition i j board
  | i > 64 = j
  | (((i>=27) && (i<=29)) || ((i>=35) && (i<=37))) && (isBlack board i) = evaluatePosition (i+1) (j + (pieceValue (PM.extractPiece (HM.lookup i board))) - 75) board
  | (((i>=27) && (i<=29)) || ((i>=35) && (i<=37))) && (isWhite board i) = evaluatePosition (i+1) (j + (pieceValue (PM.extractPiece (HM.lookup i board))) + 75) board
  | (i > 16) && (i < 49) && ((i `mod` 8) > 1) && ((i `mod` 8) < 0) && (GA.isBlack board i) = evaluatePosition (i+1) (j + (pieceValue (PM.extractPiece (HM.lookup i board))) - 50) board
  | (i > 16) && (i < 49) && ((i `mod` 8) > 1) && ((i `mod` 8) < 0) && (GA.isWhite board i) = evaluatePosition (i+1) (j + (pieceValue (PM.extractPiece (HM.lookup i board))) + 50) board
  | otherwise = evaluatePosition (i+1) (j + (pieceValue (PM.extractPiece (HM.lookup i board)))) board

pieceValue :: Piece -> Integer
pieceValue piece
  | whitePawn = 100
  | blackPawn = -100
  | whiteKnight = 300
  | blackKnight = -300
  | whiteBishop = 300
  | blackBishop = -300
  | whiteRook = 500
  | blackRook = -500
  | whiteQueen = 900
  | blackQueen = -900
  | whiteKing = 10000
  | blackKing = -10000
  | otherwise = 0
    where
      whitePawn = (piece == (Pw 1)) || (piece == (Pw 2)) || (piece == (Pw 3)) || (piece == (Pw 4)) || (piece == (Pw 5)) || (piece == (Pw 6)) || (piece == (Pw 7)) || (piece == (Pw 8))
      blackPawn = (piece == (Pb 1)) || (piece == (Pb 2)) || (piece == (Pb 3)) || (piece == (Pb 4)) || (piece == (Pb 5)) || (piece == (Pb 6)) || (piece == (Pb 7)) || (piece == (Pb 8))
      whiteKnight = (piece == (Nw 1)) || (piece == (Nw 2)) || (piece == (Nw 3))
      blackKnight = (piece == (Nb 1)) || (piece == (Nb 2)) || (piece == (Nb 3))
      whiteBishop = (piece == (Bw 1)) || (piece == (Bw 2)) || (piece == (Bw 3))
      blackBishop = (piece == (Bb 1)) || (piece == (Bb 2)) || (piece == (Bb 3))
      whiteRook = (piece == (Rw 1 True)) || (piece == (Rw 2 True)) || (piece == (Rw 3 True)) || (piece == (Rw 1 False)) || (piece == (Rw 2 False)) || (piece == (Rw 3 False))
      blackRook = (piece == (Rb 1 True)) || (piece == (Rb 2 True)) || (piece == (Rb 3 True)) || (piece == (Rb 1 False)) || (piece == (Rb 2 False)) || (piece == (Rb 3 False))
      whiteQueen = (piece == (Qw 1)) || (piece == (Qw 2)) || (piece == (Qw 3))
      blackQueen = (piece == (Qb 1)) || (piece == (Qb 2)) || (piece == (Qb 3))
      whiteKing = (piece == (Kw True True)) || (piece == (Kw True False)) || (piece == (Kw False True)) || (piece == (Kw False False)) 
      blackKing = (piece == (Kb True True)) || (piece == (Kb True False)) || (piece == (Kb False True)) || (piece == (Kb False False)) 

generateGameTree :: Integer -> (DS.Board, DS.Board2, DS.State) -> Tree (DS.Board, DS.Board2, DS.State, Integer)
generateGameTree i (b, b2, s) 
  | i == 0 = Node (b, b2, s, evaluatePosition 1 0 b) []
  | (even i) = Node (b, b2, s, minimum (P.map (extractValue) (P.map rootLabel children))) children 
  | (odd i) = Node (b, b2, s, maximum (P.map (extractValue) (P.map rootLabel children))) children 
    where
      children = P.map (generateGameTree (i-1)) (P.map (PM.processMove b b2 s) (GA.getLegalBlackMoves b b2 s))

extractValue :: (DS.Board, DS.Board2, DS.State, Integer) -> Integer
extractValue (b, b2, s, i) = i

extractBoard1 :: (DS.Board, DS.Board2, DS.State, Integer) -> DS.Board
extractBoard1 (b, b2, s, i) = b

extractBoard2 :: (DS.Board, DS.Board2, DS.State, Integer) -> DS.Board2
extractBoard2 (b, b2, s, i) = b2

extractState :: (DS.Board, DS.Board2, DS.State, Integer) -> DS.State
extractState (b, b2, s, i) = s

minimax :: Tree (DS.Board, DS.Board2, DS.State, Integer) -> (DS.Board, DS.Board2, DS.State)
minimax t 
  | ((length moves) > 0) = (PM.processMove (extractBoard1 a) (extractBoard2 a) (extractState a) (moves !! (fromIntegral i)))
  | otherwise = ((extractBoard1 a), (extractBoard2 a), (extractState a))
    where
      moves = GA.getLegalBlackMoves (extractBoard1 a) (extractBoard2 a) (extractState a)
      a = rootLabel t
      b = subForest t
      i = getMatchingChild 0 (extractValue a) b

getMatchingChild :: Integer -> Integer -> [Tree (DS.Board, DS.Board2, DS.State, Integer)] -> Integer
getMatchingChild i j (x:xs)
  | j == (extractValue (rootLabel x)) = i
  | otherwise = getMatchingChild (i+1) j xs
getMatchingChild i j [] = 0

