{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics
import Data.Default.Class

data A = A Int Double deriving (Show, Generic)
instance Default A

data B = B Int Double deriving (Show, Generic, Default)

main = do
    print (def :: A) -- A 0 0.0
    print (def :: B) -- B 0 0.0


