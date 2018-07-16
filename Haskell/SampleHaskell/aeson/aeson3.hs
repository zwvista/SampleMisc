{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Text

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving Show

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "name"
        <*> v .:? "age" .!= 20

{-
*Main> :set -XOverloadedStrings
*Main> decode "{\"name\":\"Joe\"}" :: Maybe Person
Just (Person {name = "Joe", age = 20})
-}
