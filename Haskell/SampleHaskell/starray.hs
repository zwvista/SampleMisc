-- https://stackoverflow.com/questions/8197032/starray-documentation-for-newbies-and-state-st-related-questions

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

primesUpto :: Int -> [Int]
primesUpto n = [p | (p, True) <- assocs $ sieve n]

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
    sieve <- newArray (2, n) True
    forM_ [2..n] $ \p -> do
        isPrime <- readArray sieve p
        when isPrime $ do
            forM_ [p*2, p*3 .. n] $ \k -> do
                writeArray sieve k False
    return sieve
    
main = print $ primesUpto 100