module TMVars where

import Threads (sleepMs)

import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar (newEmptyTMVar, takeTMVar, putTMVar)

main = do
    result <- atomically $ newEmptyTMVar

    forkIO $ do
        -- Pretend there is some actual work to do.
        sleepMs 5
        putStrLn "Calculated result!"
        atomically $ putTMVar result 42

    putStrLn "Waiting..."
    value <- atomically $ takeTMVar result
    putStrLn ("The answer is: " ++ show value)
