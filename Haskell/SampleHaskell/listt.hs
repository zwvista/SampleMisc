-- cabal install list-t

import Control.Monad
import Control.Monad.Trans
import ListT

myTest :: Int -> ListT IO (Int, Int)
myTest n = do
    let squares = fromFoldable . takeWhile (<=n) $ map (^(2::Int)) [0..]
    x <- squares
    y <- squares
    lift $ print (x,y)
    guard $ x + y == n
    lift $ putStrLn "Sum of squares."
    return (x,y)

main = toList $ myTest 5
