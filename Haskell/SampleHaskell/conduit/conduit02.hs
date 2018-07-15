#!/usr/bin/env stack
-- stack script --resolver lts-11.7
import Conduit
import Data.Monoid

main1 :: IO ()
main1 = print $ runConduitPure $ yieldMany [1..100 :: Int] .| sumC

main2 :: IO ()
main2 = print $ runConduitPure $ yieldMany [1..100 :: Int] .| foldlC (+) 0

main3 :: IO ()
main3 = print $ getSum $ runConduitPure $ yieldMany [1..100 :: Int] .| foldMapC Sum

main4 :: IO ()
main4 = putStrLn $ runConduitPure
     $ yieldMany [1..10 :: Int]
    .| mapC (\i -> show i ++ "\n")
    .| foldC

main5 :: IO ()
main5 = putStrLn $ runConduitPure
     $ yieldMany [1..10 :: Int]
    .| mapC show
    .| unlinesC
    .| foldC

magic6 :: Int -> IO (Product Int)
magic6 i = do
    putStrLn $ "Doing magic on " ++ show i
    return $ Product i

main6 :: IO ()
main6 = do
    Product res <- runConduit $ yieldMany [1..10] .| foldMapMC magic6
    print res

magic7 :: Int -> Int -> IO Int
magic7 total i = do
    putStrLn $ "Doing magic on " ++ show i
    return $! total * i

main7 :: IO ()
main7 = do
    res <- runConduit $ yieldMany [1..10] .| foldMC magic7 1
    print res