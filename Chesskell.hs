-The main file that runs the game

module Chesskell where

import Data.HashMap as HM
import DataStructs as DS
import GetAction as GA
import ProcessMove as PM

-----------------------------------------------------------------------------


-- Board intialization
initBoard::DS.Board
initBoard = 
  fromList ([(1,DS.Rb 1 False), (2,DS.Nb 1), (3,DS.Bb 1), (4,DS.Qb), 
            (5,DS.Kb False False), (6,DS.Bb 2), (7,DS.Nb 2),
            (8,DS.Rb 2 False), (9,DS.Pb 1), (10,DS.Pb 2), (11,DS.Pb 3), 
            (12,DS.Pb 4), (13,DS.Pb 5), (14,DS.Pb 6), (15,DS.Pb 7), 
            (16,DS.Pb 8)] ++ [(a,Empty) | a<-[17..48]] ++ 
           [(49,DS.Pw 1), (50,DS.Pw 2), (51,DS.Pw 3), (52,DS.Pw 4), 
            (53,DS.Pw 5), (54,DS.Pw 6), (55,DS.Pw 7), (56,DS.Pw 8), 
            (57,DS.Rw 1 False), (58,DS.Nw 1), (59,DS.Bw 1), (60,DS.Qw),
            (61,DS.Kw False False), (62,DS.Bw 2), (63,DS.Nw 2), (64,DS.Rw 2 False)])

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
	"Finally, you have the following options: " ,
	"   *Exit - Will exit the current game" ,
	"   *Resign - you resign the current game" , 
	"   *Help - Print this list of instructions again" ,
	"Enter your choice of game or type 'Exit':"])

printHelp :: IO ()
printHelp = putStrLn (unlines [
	"When you enter a move you should type in the square you are" ,
	"moving a piece from to the square you are moving to, E.g e2-e4",
	"Finally, you have the following options: " ,
	"   *Exit - Will exit the current game" ,
	"   *Resign - you resign the current game" , 
	"   *Help - Print this list of instructions again" ,
	"Enter your choice of game or type 'Exit':"])

parseInput :: String -> IO ()
parseInput s
  | s == "Exit" = processAction initBoard DS.Nothing DS.Exit
  | s == "1" = humanVsHuman (initBoard, DS.Nothing)
  | s == "2" = humanVsMachine1
  | s == "3" = humanVsMachine2
  | s == "4" = machineVsMachine
  | s == "Help" = printRules >> getLine >>= parseInput
  | s == "Resign" = processAction initBoard DS.Nothing DS.Resign
  | otherwise = (putStrLn "Please enter a valid input: ") >> getLine 
  			>>= parseInput

----------------------------------------------------------------------
--Human vs Human Functionality
humanVsHuman :: (DS.Board, DS.State) -> IO ()
humanVsHuman (b,s) = (printBoard 1 b) >> (GA.getAction b s) 
                      >>= (processAction b s)

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
pieceToString Qw = " Q "
pieceToString Qb = " q "
pieceToString (Kw _ _) = " K "
pieceToString (Kb _ _) = " k "
pieceToString _ = "   "


processAction :: DS.Board -> DS.State -> DS.Action -> IO ()

processAction _ _ DS.Exit = 
    putStrLn "\nEXITING... \nThank you for playing Chesskell!\n"

processAction _ _ DS.Resign = 
    putStrLn "\nYou have resigned. \nThank you for playing Chesskell!\n" 

processAction board state DS.Help = 
    printRules >> (humanVsHuman (board,state) )

processAction board state move@(DS.M _ _) = 
    humanVsHuman (PM.processMove board move)  

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


