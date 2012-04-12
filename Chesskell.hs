--The main file that runs the game

module Chesskell where

import Data.HashMap as HM
import DataStructs as DS
import GetAction as GA
import ProcessMove as PM

--------------------------------------------------------------------


-- Board intialization
initBoard::DS.Board
initBoard = 
  fromList ([(1,DS.Rb False), (2,DS.Nb), (3,DS.Bb), (4,DS.Qb), 
            (5,DS.Kb False False), (6,DS.Bb), (7,DS.Nb),
             (8,DS.Rb False), (9,DS.Pb), (10,DS.Pb), (11,DS.Pb), 
           (12,DS.Pb), (13,DS.Pb), (14,DS.Pb), (15,DS.Pb), (16,DS.Pb)]
           ++ [(a,Empty) | a<-[17..48]] ++ 
          [(49,DS.Pw), (50,DS.Pw), (51,DS.Pw), (52,DS.Pw), (53,DS.Pw), 
           (54,DS.Pw), (55,DS.Pw), (56,DS.Pw), (57,DS.Rw False),
             (58,DS.Nw), (59,DS.Bw), (60,DS.Qw), (61,DS.Kw False False),
              (62,DS.Bw), (63,DS.Nw), (64,DS.Rw False)])

--General IO Parsing to Start Everything 
main :: IO ()
main = printRules >> getLine >>= parseInput


printRules :: IO ()
printRules = putStr (unlines ["\nWelcome to Chesskell!!!" , 
	"Rules: " , 
	"You can choose between the following options" ,
	"1) Human vs. Human - Where 2 human players enter moves" ,
	"2) Human vs. Machine1 - Human plays against minimax machine" ,
	"3) Human vs. Machine2 - Human plays against alphabeta machine",
	"4) Machine1 vs. Machine2 - Pit machines against each other\n" ,
	"When you enter a move you should type in the square you are" ,
	"moving a piece from to the square you are moving to, E.g e2-e4",
	"Finally, you have the following options: " ,
	"-Exit - Will exit the current game" ,
	"-Resign - you resign the current game" , 
	"-Help - Print this list of instructions again" ,
	"\nEnter your choice of game or type exit: "])

printHelp :: IO ()
printHelp = putStrLn (unlines [
	"When you enter a move you should type in the square you are" ,
	"moving a piece from to the square you are moving to, E.g e2-e4",
	"Finally, you have the following options: " ,
	"-Exit - Will exit the current game" ,
	"-Resign - you resign the current game" , 
	"-Help - Print this list of instructions again" ,
	"Enter your choice of game or type exit: "])

parseInput :: String -> IO ()
parseInput s
  | s == "Exit" = putStrLn "Bye!"
  | s == "1" = humanVsHuman (initBoard, DS.Nothing)
  | s == "2" = humanVsMachine1
  | s == "3" = humanVsMachine2
  | s == "4" = machineVsMachine
  | s == "Help" = printRules >> getLine >>= parseInput
  | s == "Resign" = putStrLn "You lost, but it was a good game"
  | otherwise = (putStrLn "Please enter a valid input: ") >> getLine 
  			>>= parseInput

----------------------------------------------------------------------
--Human vs Human Functionality
humanVsHuman :: (DS.Board, DS.State) -> IO ()
humanVsHuman (b,s) = (printBoard 1 b) >> (GA.getAction b s) 
                      >>= (processAction b s)

printBoard :: Integer -> Board -> IO ()
printBoard x b
  | x == 1 = putStr "8" >> (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b)
  | x == 9 = putStr "7" >> (putStr ("|" ++ (pieceToString (b ! x)) )) >> (printBoard (x+1) b)
  | x == 17 = putStr "6" >> (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b)
  | x == 25 = putStr "5" >> (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b)
  | x == 33 = putStr "4" >> (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b)
  | x == 41 = putStr "3" >> (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b)
  | x == 49 = putStr "2" >> (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b)
  | x == 57 = putStr "1" >> (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b)
  | (x <= 64) && ((x `mod` 8) == 0) = (putStrLn ("|" ++ (pieceToString (b ! x)) ++ "|")) >> (printBoard (x+1) b)
  | (x <= 64) = (putStr ("|" ++ (pieceToString (b ! x)))) >> (printBoard (x+1) b) 
  | otherwise = (putStrLn "  a b c d e f g h")

pieceToString :: Piece -> String
pieceToString (Rw _) = "R"
pieceToString (Rb _) = "r"
pieceToString (Kw _ _) = "K"
pieceToString (Kb _ _) = "k"
pieceToString p
  | p == Pw = "P"
  | p == Pb = "p"
  | p == Nw  = "N"
  | p == Nb = "n"
  | p == Bw = "B"
  | p == Bb = "b"
  | p == Qw = "Q"
  | p == Qb = "q"
  | otherwise = " "


processAction :: DS.Board -> DS.State -> DS.Action -> IO ()

processAction board _ DS.Exit = 
    putStrLn "\n\nExiting... \nThank you for playing Chesskell!\n"

processAction board _ DS.Resign = 
    putStrLn "\n\nYou have resigned. \nThank you for playing Cesskell!\n" 

processAction board state DS.Help = 
    printRules >> (humanVsHuman (board,state) )

processAction board state move@(DS.M _ _) = 
    humanVsHuman (PM.processMove board move)  

---------------------------------------------------------------------
--Human vs. Machine1 Functionality
humanVsMachine1 :: IO ()
humanVsMachine1 = putStrLn "HumanvMachine1"

--------------------------------------------------------------------
--Human vs. Machine2 Functionality
humanVsMachine2 :: IO ()
humanVsMachine2 = putStrLn "HumanvMachine2"

--------------------------------------------------------------------
--Machine vs. Machine Functionality
machineVsMachine :: IO ()
machineVsMachine = putStrLn "MachinevMachine!"


