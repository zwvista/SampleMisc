-- https://stackoverflow.com/questions/26385809/catch-someexception-with-exceptt
-- cabal install lifted-base

import Control.Exception.Lifted
import Control.Monad.Trans.Except

badFunction :: ExceptT SomeException IO ()
badFunction = throw DivideByZero

intercept
  :: ExceptT SomeException IO a
  -> ExceptT SomeException IO a
intercept a = do
  r <- try $ a
  case r of
    Right x -> return x
    Left e -> throwE e

intercept'
  :: ExceptT SomeException IO a
  -> ExceptT SomeException IO a
intercept' = handle throwE

main :: IO ()
main = do
    r <- runExceptT $ intercept badFunction
    case r of Left _ -> putStrLn "caught error"
              Right _ -> putStrLn "nope, didn't catch no error" 
