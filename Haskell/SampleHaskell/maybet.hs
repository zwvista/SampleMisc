import Control.Monad             (guard)
import Control.Monad.Trans       (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Data.Char

funA :: MaybeT IO String
funA = do
    lift $ putStrLn "What is your name?"
    name <- lift getLine
    guard $ not (null name)
    return name

funB :: String -> MaybeT IO String
funB name = do
    lift $ putStrLn ("hello, " ++ name)
    lift $ putStrLn "how old are you?"
    age <- lift getLine
    guard (all isDigit age)
    return age

main :: IO (Maybe String)
main = runMaybeT $ do
    a <- funA
    funB a
