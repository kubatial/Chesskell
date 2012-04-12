module ProcessMove where
import DataStructs as DS

processMove :: DS.Board -> DS.Action -> (DS.Board, DS.State)
processMove board (DS.M s d) = (board, DS.Nothing)