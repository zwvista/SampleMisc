#!/usr/bin/env stack
-- stack script --resolver lts-11.7
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main1 :: IO ()
main1 = runConduit $ yield 1 .| mapM_C print

main2 :: IO ()
main2 = runConduit $ (yield 1 >> yield 2) .| mapM_C print

main3 :: IO ()
main3 = do
    -- prints: Just 1
    print $ runConduitPure $ yield 1 .| await
    -- prints: Nothing
    print $ runConduitPure $ yieldMany [] .| await

    -- Note, that the above is equivalent to the following. Work out
    -- why this works:
    print $ runConduitPure $ return () .| await
    print $ runConduitPure await

myMapC :: Monad m => (i -> o) -> ConduitT i o m ()
myMapC f =
    loop
  where
    loop = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x -> do
                yield (f x)
                loop

main4 :: IO ()
main4 = runConduit $ yieldMany [1..10] .| myMapC (+ 1) .| mapM_C print

main5 :: IO ()
main5 = print $ runConduitPure $ yieldMany [1..10] .| do
    x <- takeWhileC (<= 5) .| sinkList
    y <- sinkList
    return (x, y)

myTakeWhileC :: Monad m => (i -> Bool) -> ConduitT i i m ()
myTakeWhileC f =
    loop
  where
    loop = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x
                | f x -> do
                    yield x
                    loop
                | otherwise -> return ()

main6 :: IO ()
main6 = print $ runConduitPure $ yieldMany [1..10] .| do
    x <- myTakeWhileC (<= 5) .| sinkList
    y <- sinkList
    return (x, y)

myGoodTakeWhileC :: Monad m => (i -> Bool) -> ConduitT i i m ()
myGoodTakeWhileC f =
    loop
  where
    loop = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x
                | f x -> do
                    yield x
                    loop
                | otherwise -> leftover x

main7 :: IO ()
main7 = print $ runConduitPure $ yieldMany [1..10] .| do
    x <- myGoodTakeWhileC (<= 5) .| sinkList
    y <- sinkList
    return (x, y)

main8 :: IO ()
main8 = print $ runConduitPure $ return () .| do
    mapM_ leftover [1..10]
    sinkList