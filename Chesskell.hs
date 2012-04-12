--The main file that runs the game

module Chesskell where

import Data.HashMap
import DataStructs

--------------------------------------------------------------------


-- Board intialization
initBoard::Board
initBoard = 
  fromList ([(1,Rb False), (2,Nb), (3,Bb), (4,Qb), (5,Kb False False), 
           (6,Bb), (7,Nb), (8,Rb False), (9,Pb), (10,Pb), (11,Pb), 
           (12,Pb), (13,Pb), (14,Pb), (15,Pb), (16,Pb)] ++
           [(a,Empty) | a<-[17..48]] ++ 
          [(49,Pw), (50,Pw), (51,Pw), (52,Pw), (53,Pw), (54,Pw), 
           (55,Pw), (56,Pw), (57,Rw False), (58,Nw), (59,Bw),
            (60,Qw), (61,Kw False False), (62,Bw), (63,Nw), (64,Rw False)])

--General IO Parsing to Start Everything 
main :: IO ()
main = printRules >> getLine >>= parseInput


printRules :: IO ()
printRules = putStrLn (unlines ["\nWelcome to Chesskell!!!" , 
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
	"Enter your choice of game or type exit: "])

parseInput :: String -> IO ()
parseInput s
  | s == "Exit" = putStrLn "Bye!"
  | s == "1" = humanVsHuman
  | s == "2" = humanVsMachine1
  | s == "3" = humanVsMachine2
  | s == "4" = machineVsMachine
  | s == "Help" = printRules >> getLine >>= parseInput
  | s == "Resign" = putStrLn "You lost, but it was a good game"
  | otherwise = (putStrLn "Please enter a valid input: ") >> getLine 
  			>>= parseInput

----------------------------------------------------------------------
--Human vs Human Functionality
humanVsHuman :: IO ()
humanVsHuman = putStrLn "HumanvHuman"

--humanVsHuman = (printBoard 1 initBoard) >> (putStr "Enter Move: ")
--		>>= parseMove

--printBoard :: Int -> Board -> IO ()
--printBoard x b
--  | (x <= 64) && ((x `mod` 8) == 0) = putStrLn (pieceToString (b ! x))
--  | (x <= 64) = putStr (pieceToString (b ! x)) ++ 

--parseMove :: String -> IO ()


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

