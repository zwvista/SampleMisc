import System.IO
import Data.Char

inputPath = "input.txt"
outputPath = "output.txt"
openFile1 = do
    handle <- openFile inputPath ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
withFile1 = do
    withFile inputPath ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
readFile1 = do
    contents <- readFile inputPath
    putStr contents

readWriteUTF8File = do
    inputHandle <- openFile inputPath ReadMode 
    hSetEncoding inputHandle utf8
    theInput <- hGetContents inputHandle
    outputHandle <- openFile outputPath WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle $ map toUpper theInput
    hClose inputHandle
    hClose outputHandle

