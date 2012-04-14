module GetAction where
import DataStructs as DS

getAction :: DS.Board -> DS.State -> IO DS.Action
getAction board state = putStrLn "Enter Move: " >> getLine >>=  parseUserMove --return DS.Exit

parseUserMove :: String -> IO DS.Action
parseUserMove move
  | (move == "Exit") || (move == "exit") = return DS.Exit
  | (move == "Resign") || (move == "resign") = return DS.Resign
  | (move == "Help") || (move == "help") = return DS.Help
  | (length move) /= 5 = putStrLn "Please Enter Your Input as From-To " >> getLine >>= parseUserMove
  | otherwise = return (M (convertToInt (take 2 move)) (convertToInt (drop 3 move)))

convertToInt :: String -> Integer
convertToInt ('a':x) = 1 + (8-(read x :: Integer))*8
convertToInt ('b':x) = 2 + (8-(read x :: Integer))*8
convertToInt ('c':x) = 3 + (8-(read x :: Integer))*8
convertToInt ('d':x) = 4 + (8-(read x :: Integer))*8
convertToInt ('e':x) = 5 + (8-(read x :: Integer))*8
convertToInt ('f':x) = 6 + (8-(read x :: Integer))*8
convertToInt ('g':x) = 7 + (8-(read x :: Integer))*8
convertToInt ('h':x) = 8 + (8-(read x :: Integer))*8

