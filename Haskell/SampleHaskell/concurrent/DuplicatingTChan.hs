module DuplicatingTChan where

import Threads (sleepMs)

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (newTChan, writeTChan, readTChan, dupTChan)

nonDuplicatedTest = do
    messages <- atomically newTChan
    forkIO $ messageReader messages "First"
    forkIO $ messageReader messages "Second"
    atomically $ writeTChan messages "Hi!"

messageReader channel name = do
    msg <- atomically $ readTChan channel
    putStrLn (name ++ " read: " ++ msg)

duplicatedTest = do
    broadcast <- atomically newTChan
    forkIO $ broadcastReader broadcast "Third"
    forkIO $ broadcastReader broadcast "Fourth"
    sleepMs 1
    atomically $ writeTChan broadcast "Bye!"

broadcastReader channel name = do
    channel' <- atomically $ dupTChan channel
    messageReader channel' name

main = do
    nonDuplicatedTest
    duplicatedTest
    sleepMs 10
