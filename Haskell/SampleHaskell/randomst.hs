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
    
threeCoins3 :: IO (Bool, Bool, Bool)
threeCoins3 = liftM3 (,,) randomIO randomIO randomIO

main = do
    gen <- newStdGen
    print $ evalState threeCoins gen
    gen <- newStdGen
    print $ evalState threeCoins2 gen
    threeCoins3
