module TChan2 where

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

oneSecond = 1000000

writerThread :: TChan Int -> IO ()
writerThread chan = do
        atomically $ writeTChan chan 1
        threadDelay oneSecond
        atomically $ writeTChan chan 2
        threadDelay oneSecond
        atomically $ writeTChan chan 3
        threadDelay oneSecond

readerThread :: TChan Int -> IO ()
readerThread chan = do
        newInt <- atomically $ readTChan chan
        putStrLn $ "read new value: " ++ show newInt
        readerThread chan

main = do
        chan <- atomically $ newTChan
        forkIO $ readerThread chan
        forkIO $ writerThread chan
        threadDelay $ 5 * oneSecond