import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)
    
threeCoins2 :: State StdGen (Bool,Bool,Bool)
threeCoins2 = liftM3 (,,) randomSt randomSt randomSt

main = print $ evalState threeCoins2 (mkStdGen 100)
