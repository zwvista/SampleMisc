{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import GHC.Generics
import Data.Aeson
import Data.Text
import Control.Monad

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)

data Persons = Persons {
      persons :: [Person]
    } deriving (Generic, Show)

instance ToJSON Persons where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Persons where
    parseJSON (Object o) = Persons <$> o .: "persons"
    parseJSON _ = mzero

instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person where
    parseJSON (Object o) = Person <$> o .: "name" <*> o .: "age"
    parseJSON _ = mzero

{-
>>> encode (Persons {persons = [Person {name = "Joe", age = 12}]})
"{\"persons\":[{\"name\":\"Joe\",\"age\":12}]}"
>>> decode "{\"persons\":[{\"name\":\"Joe\",\"age\":12}]}" :: Maybe Persons
Just (Persons {persons = [Person {name = "Joe", age = 12}]})
-}
