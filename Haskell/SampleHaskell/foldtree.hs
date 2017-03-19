import qualified Data.Foldable as F
import Data.Monoid

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
                             
testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )

main = do
    -- print $ F.foldl (+) 0 testTree
    print $ F.sum testTree
    -- print $ F.foldl (*) 1 testTree
    print $ F.product testTree
    print $ getAny $ F.foldMap (\x -> Any $ x == 3) testTree
    print $ getAny $ F.foldMap (\x -> Any $ x > 15) testTree
    -- print $ F.foldMap (\x -> [x]) testTree
    print $ F.toList testTree
    print $ F.length testTree
    print $ F.maximum testTree
    print $ F.minimum testTree
    print $ 3 `F.elem` testTree
    print $ F.null testTree
      
{-
42
64800
True
False
[1,3,6,5,8,9,10]
7
10
1
True
False
-}