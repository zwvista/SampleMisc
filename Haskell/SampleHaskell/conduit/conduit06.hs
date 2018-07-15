#!/usr/bin/env stack
-- stack script --resolver lts-11.7
import Conduit

main1 :: IO ()
main1 = runConduit $ yieldMany [1..10] .| iterMC print .| return ()

main2 :: IO ()
main2 = runConduit $ yieldMany [1..10] .| iterMC print .| sinkNull

main3 :: IO ()
main3 = runConduit $ yieldMany [1..10] .| iterMC print .| return () .| sinkNull

main4 :: IO ()
main4 = runConduit
     $ yieldMany [1..10]
    .| iterMC print
    .| liftIO (putStrLn "I was called")
    .| sinkNull
