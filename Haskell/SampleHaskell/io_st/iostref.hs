import Control.Monad.ST
import Data.STRef
import Data.IORef

counterIO :: IO (IO Int)
counterIO = do
    c <- newIORef 0
    return $ do
        modifyIORef c (+1)
        readIORef c

counterST :: ST s (ST s Int)
counterST = do
    c <- newSTRef 0
    return $ do
        modifySTRef c (+1)
        readSTRef c

main = do
    f <- counterIO
    print =<< f
    print =<< f
    print =<< f
    print $ runST $ do
        f2 <- counterST
        x <- f2
        y <- f2
        z <- f2
        return (x, y, z)
    
{-    
1
2
3
(1,2,3)
-}
