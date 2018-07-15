#!/usr/bin/env stack
-- stack script --resolver lts-11.7

import Conduit

source :: Monad m => ConduitT i Int m ()
source = do
    yieldMany [1..10]
    yieldMany [11..20]

main1 :: IO ()
main1 = runConduit $ source .| mapM_C print

sink :: Monad m => ConduitT Int o m (String, Int)
sink = do
    x <- takeC 5 .| mapC show .| foldC
    y <- sumC
    return (x, y)

main2 :: IO ()
main2 = do
    let res = runConduitPure $ yieldMany [1..10] .| sink
    print res

trans :: Monad m => ConduitT Int Int m ()
trans = do
    takeC 5 .| mapC (+ 1)
    mapC (* 2)

main3 :: IO ()
main3 = runConduit $ yieldMany [1..10] .| trans .| mapM_C print