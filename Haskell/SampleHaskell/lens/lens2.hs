-- Some of the examples in this chapter require a few GHC extensions:
-- TemplateHaskell is needed for makeLenses; RankNTypes is needed for
-- a few type signatures later on. 
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Control.Lens

data NewTask =
  SimpleTask String |
  HarderTask String Int |
  CompoundTask String [NewTask]
  deriving (Show)

makePrisms ''NewTask

a = SimpleTask "Clean"
b = HarderTask "Clean Kitchen" 15
c = CompoundTask "Clean House" [a,b]

{-
>> a ^? _SimpleTask
Just “Clean”
>> b ^? _HarderTask
Just (“Clean Kitchen”, 15)
>> a ^? _HarderTask
Nothing
>> c ^? _SimpleTask
Nothing

>> b ^? _HarderTask._2
Just 15
>> c ^? _CompoundTask._1
Just “Clean House”
>> a ^? _HarderTask._1
Nothing

>> b & _SimpleTask .~ "Clean Garage"
HarderTask “Clean Kitchen" 15
>> b & _HarderTask._2 .~ 30
HarderTask “Clean Kitchen" 30
-}