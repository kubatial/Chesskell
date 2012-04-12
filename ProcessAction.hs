module ProcessAction where
import DataStructs

processAction :: Board -> Action -> IO ()
processAction board act = putStrLn "Call to processAction"