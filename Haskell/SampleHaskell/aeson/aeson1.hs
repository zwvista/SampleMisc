{-# LANGUAGE DeriveGeneric #-}

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

instance ToJSON Person
instance FromJSON Person

instance ToJSON Persons
instance FromJSON Persons

{-
*Main> :set -XOverloadedStrings
*Main> encode (Person {name = "Joe", age = 12})
"{\"name\":\"Joe\",\"age\":12}"
*Main> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
Just (Person {name = "Joe", age = 12})
*Main> encode (Persons {persons = [Person {name = "Joe", age = 12}]})
"{\"persons\":[{\"name\":\"Joe\",\"age\":12}]}"
*Main> decode "{\"persons\":[{\"name\":\"Joe\",\"age\":12}]}" :: Maybe Persons
Just (Persons {persons = [Person {name = "Joe", age = 12}]})
-}
