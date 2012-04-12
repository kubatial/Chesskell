module ProcessAction where
import DataStructs

processAction :: Board -> Action -> IO ()
processAction board act = putStrLn (unwords ["Call to processAction with action:", show act])