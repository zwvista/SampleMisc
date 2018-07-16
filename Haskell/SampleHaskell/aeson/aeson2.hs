{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Text

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving Show

data Persons = Persons {
      persons :: [Person]
    } deriving Show

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "name"
        <*> v .: "age"

instance ToJSON Person where
    toJSON (Person name age) =
        object ["name" .= name, "age" .= age]

instance FromJSON Persons where
    parseJSON = withObject "Persons" $ \v -> Persons
        <$> v .: "persons"

instance ToJSON Persons where
    toJSON (Persons persons) =
        object ["persons" .= persons]

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