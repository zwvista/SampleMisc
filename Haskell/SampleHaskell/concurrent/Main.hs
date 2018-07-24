-- Main entry point to the application.
module Main where

import qualified Threads (main)
import qualified TMVars (main)
import qualified TMVarSharedState (main)
import qualified TVarSharedState (main)
import qualified TChan1 (main)
import qualified TChan2 (main)
import qualified DuplicatingTChan (main)

-- The main entry point.
main :: IO ()
main = do
    example "Threads" Threads.main
    example "TMVar" TMVars.main
    example "Shared state 1" TMVarSharedState.main
    example "Shared state 2" TVarSharedState.main
    example "Channels 1" TChan1.main
    example "Channels 2" TChan2.main
    example "Duplicating channels" DuplicatingTChan.main

example title code = do
    putStrLn ("~~~ Running " ++ title ++ " example! ~~~")
    code
    putStrLn ""
