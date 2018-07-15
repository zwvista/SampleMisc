#!/usr/bin/env stack
-- stack script --resolver lts-11.7
import Conduit
import qualified System.IO as IO

main1 :: IO ()
main1 = IO.withBinaryFile "input.txt" IO.ReadMode $ \inH ->
       IO.withBinaryFile "output.txt" IO.WriteMode $ \outH ->
       runConduit $ sourceHandle inH .| sinkHandle outH

main2 :: IO ()
main2 = withSourceFile "input.txt" $ \source ->
       withSinkFile "output.txt" $ \sink ->
       runConduit $ source .| sink