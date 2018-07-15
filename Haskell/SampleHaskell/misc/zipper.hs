import Data.Char

data Direction = L | R deriving (Show)
type Directions = [Direction]
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

changeTop :: Directions -> Tree Char -> Tree Char
changeTop (L:ds) (Node x l r)   = Node x (changeTop ds l) r
changeTop (R:ds) (Node x l r)   = Node x l (changeTop ds r) 
changeTop [] (Node _ l r)       = Node 'P' l r

elemAt :: Directions -> Tree Char -> Char
elemAt (L:ds) (Node _ l _)      = elemAt ds l
elemAt (R:ds) (Node _ _ r)      = elemAt ds r
elemAt [] (Node x _ _)          = x

freeTree :: Tree Char
freeTree = 
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W' 
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

-- | 関数のチェーンを見やすくするための演算子を定義
-- | x -: f -: f -: ..
(-:) :: a -> (a -> b) -> b
(-:) x f = f x


-- | 型定義：値コンストラクタ 親ノードの要素 選ばなかった部分木 
data Crumb a    =    LeftCrumb a (Tree a)
                |   RightCrumb a (Tree a)
                deriving(Show)
-- | 新しいパンくずリスト
type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft ((Node x l r), bs)   = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight ((Node x l r), bs)   = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs)      = (Node x t r, bs)
goUp (t, RightCrumb x l:bs)     = (Node x l t, bs)

-- 上記の関数は、パターンマッチに失敗すると実行時エラーが出てしまう。
-- 自作：goUpについて、空のパンくずが与えられてもエラーにならないように、Maybeモナドを使うならこのようにする
goUp' :: (Tree a, Breadcrumbs a) -> Maybe (Tree a, Breadcrumbs a)
goUp' (t, []) = Nothing
goUp' (t, LeftCrumb x r:bs)      = return (Node x t r, bs)
goUp' (t, RightCrumb x l:bs)     = return (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f ((Node x l r), bs) = (Node (f x) l r, bs)
modify f (Empty, bs)        = (Empty, bs)

main = print $ (freeTree, []) -: goLeft -: goRight -: goUp -: modify toLower 

-- (Node 'o' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),[LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])

