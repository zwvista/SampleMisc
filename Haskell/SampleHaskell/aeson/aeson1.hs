{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import GHC.Generics
import Data.Aeson
import Data.Text

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

{-
>>> encode (Person {name = "Joe", age = 12})
"{\"name\":\"Joe\",\"age\":12}"
>>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
Just (Person {name = "Joe", age = 12})
-}