import System.IO

txt = "readfile.txt"
openFile1 = do
    handle <- openFile txt ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
withFile1 = do
    withFile txt ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
readFile1 = do
    contents <- readFile txt
    putStr contents
