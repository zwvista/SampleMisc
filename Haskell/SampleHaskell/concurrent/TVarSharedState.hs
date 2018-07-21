module TVarSharedState where

import Threads (sleepMs)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar)
import Control.Monad (replicateM)

main = do
    counter <- atomically $ newTVar 0

    let increment = atomically $ do
            count <- readTVar counter
            writeTVar counter $! count + 1
        incrementer = do
            replicateM 1000 increment
            return ()

    threads <- replicateM 5 (forkIO incrementer)

    sleepMs 100
    count <- atomically $ readTVar counter
    print count
