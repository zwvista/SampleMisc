#!/usr/bin/env stack
-- stack script --resolver lts-11.7
import Conduit

main1 :: IO ()
main1 = runConduit $ yieldMany [1..10] .| mapC (* 2) .| mapM_C print

main2 :: IO ()
main2 = runConduit $ yieldMany [1..10] .| filterC even .| mapM_C print

main3 :: IO ()
main3 = runConduit $ yieldMany [1..10] .| intersperseC 0 .| mapM_C print

main4 :: IO ()
main4 = runConduit
     $ yieldMany (map (replicate 5) [1..10])
    .| concatC
    .| mapM_C print

evenM :: Int -> IO Bool
evenM i = do
    let res = even i
    print (i, res)
    return res

main5 :: IO ()
main5 = runConduit
     $ yieldMany [1..10]
    .| filterMC evenM
    .| mapM_C print

main6 :: IO ()
main6 = do
    res <- runConduit $ yieldMany [1..10] .| iterMC print .| sumC
    print res