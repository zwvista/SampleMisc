import Control.Monad.Trans.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd'' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

main = do
    mapM_ putStrLn . execWriter $ gcd' 8 3
    mapM_ putStrLn . fromDiffList . execWriter $ gcd'' 8 3

{--
8 mod 3 = 2
3 mod 2 = 1
2 mod 1 = 0
Finished with 1
Finished with 1
2 mod 1 = 0
3 mod 2 = 1
8 mod 3 = 2
--}