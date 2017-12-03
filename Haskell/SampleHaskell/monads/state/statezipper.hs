import Control.Monad.State
import Data.List.Zipper

f :: State (Zipper Int) [Int]
f = do
    modify right -- [3],[4,5]
    modify $ replace 6 -- [3],[6,5]
    modify $ insert 7 -- [3],[7,6,5]
    gets toList -- [3,7,6,5]

main = print . evalState f $ fromList [3,4,5]

