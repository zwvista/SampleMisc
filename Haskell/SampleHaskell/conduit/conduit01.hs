#!/usr/bin/env stack
-- stack script --resolver lts-11.7
import Conduit

main1 :: IO ()
main1 = do
    -- Pure operations: summing numbers.
    print $ runConduitPure $ yieldMany [1..10] .| sumC

    -- Exception safe file access: copy a file.
    writeFile "input.txt" "This is a test." -- create the source file
    runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt" -- actual copying
    readFile "output.txt" >>= putStrLn -- prove that it worked

    -- Perform transformations.
    print $ runConduitPure $ yieldMany [1..10] .| mapC (+ 1) .| sinkList

main2 :: IO ()
main2 = do
    putStrLn "List version:"
    print $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    print $ runConduitPure $ yieldMany [1..] .| takeC 10 .| sinkList

main3 :: IO ()
main3 = do
    putStrLn "List version:"
    print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    print $ runConduitPure
          $ yieldMany [1..]
         .| takeC 10
         .| mapC (* 2)
         .| takeWhileC (< 18)
         .| sinkList

main4 :: IO ()
main4 = do
    putStrLn "List version:"
    mapM_ print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    runConduit
          $ yieldMany [1..]
         .| takeC 10
         .| mapC (* 2)
         .| takeWhileC (< 18)
         .| mapM_C print

magic :: Int -> IO Int
magic x = do
    putStrLn $ "I'm doing magic with " ++ show x
    return $ x * 2

main5 :: IO ()
main5 = do
    putStrLn "List version:"
    mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18)
    putStrLn ""
    putStrLn "Conduit version:"
    runConduit
          $ yieldMany [1..]
         .| takeC 10
         .| mapMC magic
         .| takeWhileC (< 18)
         .| mapM_C print