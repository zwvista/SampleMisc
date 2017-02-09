import Control.Monad.State

fact_state :: State Int Int
fact_state = get >>= \x -> if x <= 1
                           then return 1
                           else (put (x - 1) >> fmap (*x) fact_state)

factorial :: Int -> Int
factorial = evalState fact_state

fibs_state :: State (Int, Int, Int) Int
fibs_state = get >>= \(x1, x2, n) -> if n == 0
                                     then return x1
                                     else (put (x2, x1+x2, n-1) >> fibs_state)

fibonacci n = evalState fibs_state (0, 1, n)

main = do
    print $ factorial <$> [1..10]
    print $ fibonacci <$> [1..10]
