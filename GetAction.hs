module GetAction where
import DataStructs as DS

getAction :: DS.Board -> DS.State -> IO DS.Action
getAction board state = return DS.Exit