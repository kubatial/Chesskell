--The main file that runs the game

module Chesskell where

import Data.HashMap as HM
import DataStructs as DS
import GetAction as GA
import ProcessMove as PM

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
	"3) Human vs. Machine2 - Human plays against alphabeta machine",
	"4) Machine1 vs. Machine2 - Pit machines against each other\n" ,
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
  | (s == "Exit") || (s == "exit")  = processAction initBoard initBoard2 DS.NothingWhite DS.Exit
  | s == "1" = humanVsHuman (initBoard, initBoard2, DS.NothingWhite)
  | s == "2" = humanVsMachine1
  | s == "3" = humanVsMachine2
  | s == "4" = machineVsMachine
  | (s == "Help") || (s == "help")  = printRules >> getLine >>= parseInput
  | (s == "Resign") || (s == "resign") = processAction initBoard initBoard2 DS.NothingWhite DS.Resign
  | otherwise = (putStrLn "Please enter a valid input: ") >> getLine 
  			>>= parseInput

----------------------------------------------------------------------
--Human vs Human Functionality
humanVsHuman :: (DS.Board, DS.Board2, DS.State) -> IO ()
humanVsHuman (b, b2, s) = (printBoard 1 b) >> (GA.getAction b b2 s) 
                      >>= (processAction b b2 s)

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


processAction :: DS.Board -> DS.Board2 -> DS.State -> DS.Action -> IO ()

processAction _ _ _ DS.Exit = 
    putStrLn "\nEXITING... \nThank you for playing Chesskell!\n"

processAction _ _ _ DS.Resign = 
    putStrLn "\nYou have resigned. \nThank you for playing Chesskell!\n" 

processAction _ _ DS.WhiteCheckmate _ = 
    putStrLn "\nBlack wins. \n Thank you for playing Chesskell!\n"
   
processAction _ _ DS.BlackCheckmate _ = 
    putStrLn "\nWhite wins. \n Thank you for playing Chesskell!\n"

processAction _ _ DS.Stalemate _ = 
    putStrLn "\nThe game ended in a Stalemate. \n Thank you for playing Chesskell!\n"

processAction board board2 state DS.Help = 
    printRules >> (humanVsHuman (board, board2, state) )

processAction board board2 state move = 
    humanVsHuman (PM.processMove board board2 state move)  

-----------------------------------------------------------------------------
--Human vs. Machine1 Functionality
humanVsMachine1 :: IO ()
humanVsMachine1 = putStrLn "HumanvMachine1"

-----------------------------------------------------------------------------
--Human vs. Machine2 Functionality
humanVsMachine2 :: IO ()
humanVsMachine2 = putStrLn "HumanvMachine2"

-----------------------------------------------------------------------------
--Machine vs. Machine Functionality
machineVsMachine :: IO ()
machineVsMachine = putStrLn "MachinevMachine!"


