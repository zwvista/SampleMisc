import Control.Monad
import Control.Monad.Trans.Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False
        
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

withdraw :: Int -> Int -> Writer [String] Int
withdraw balance nextWithdrawal
    | nextWithdrawal <= balance = do
        let nextBalance = balance - nextWithdrawal
        tell ["Withdrew " ++ show nextWithdrawal ++ " Balance: " ++ show nextBalance]
        return nextBalance
    | otherwise = do
        tell ["Cannot withdraw " ++ show nextWithdrawal]
        return balance

main = do
    let (a, w) = runWriter $ filterM keepSmall [9,1,5,2,10,3]
    mapM_ putStrLn w; print a
    print $ powerset [1,2,3]
    let (a, w) = runWriter $ foldM withdraw 100 [20, 10, 40, 50, 10, 70, 30]
    mapM_ putStrLn w; print a
