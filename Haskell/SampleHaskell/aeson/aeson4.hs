{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import GHC.Generics
import Data.Aeson
import Data.Text

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)

data Persons = Persons {
      persons :: [Person]
    } deriving (Generic, Show)

instance ToJSON Persons where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Persons

instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

{-
>>> encode (Persons {persons = [Person {name = "Joe", age = 12}]})
"{\"persons\":[{\"name\":\"Joe\",\"age\":12}]}"
>>> decode "{\"persons\":[{\"name\":\"Joe\",\"age\":12}]}" :: Maybe Persons
Just (Persons {persons = [Person {name = "Joe", age = 12}]})
-}
