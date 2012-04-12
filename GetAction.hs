module GetAction where
import DataStructs

getAction :: Board -> State -> Action
getAction board state = Exit