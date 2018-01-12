{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import GHC.Generics
import Data.Aeson
import Data.Text

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving Show

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "name"
        <*> v .: "age"

instance ToJSON Person where
    toJSON (Person name age) =
        object ["name" .= name, "age" .= age]

    toEncoding (Person name age) =
        pairs ("name" .= name <> "age" .= age)

{-
>>> encode (Person {name = "Joe", age = 12})
"{\"name\":\"Joe\",\"age\":12}"
>>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
Just (Person {name = "Joe", age = 12})
-}