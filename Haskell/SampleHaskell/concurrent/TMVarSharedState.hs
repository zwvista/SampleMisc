module TMVarSharedState where

import Threads (sleepMs)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newTMVar, takeTMVar, putTMVar)
import Control.Monad (replicateM)

main = do
    counter <- atomically $ newTMVar 0

    let increment = do
            count <- atomically $ takeTMVar counter
            atomically $ putTMVar counter $! count + 1
        incrementer = do
            replicateM 1000 increment
            return ()

    threads <- replicateM 5 (forkIO incrementer)

    sleepMs 100
    count <- atomically $ takeTMVar counter
    print count
