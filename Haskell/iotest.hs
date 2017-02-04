import Data.Char

main = do
    putStrLn "What is you choice， 1 or 2?"
    choice <- getLine
    menu choice

menu "1" = main1
menu "2" = main2
menu _ = return ()

main1 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    let bigName = map toUpper name
    putStrLn ("Hey " ++ bigName ++ ", how are you?")

main2 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main2

reverseWords :: String -> String
reverseWords = unwords . map reverse . words