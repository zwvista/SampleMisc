module TChan1 where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChan, writeTChan, readTChan)

main = do
    messages <- atomically newTChan
    atomically $ writeTChan messages "unbounded"
    atomically $ writeTChan messages "channels"

    -- Read a message from the channel, then output it.
    msg <- atomically $ readTChan messages
    putStrLn msg
    -- Do the same thing again, but more concisely.
    putStrLn =<< (atomically $ readTChan messages)
