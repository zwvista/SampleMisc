{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data Person = Person {
      field_NAME :: Text
    , field_AGE  :: Int
    } deriving (Generic, Show)

customOptions = defaultOptions
                { fieldLabelModifier = drop $ length ("field_" :: String)
                }

instance ToJSON Person where
    toJSON = genericToJSON customOptions
instance FromJSON Person where
    parseJSON = genericParseJSON customOptions

{-
*Main> :set -XOverloadedStrings
*Main> encode (Person {field_NAME = "Joe", field_AGE = 12})
"{\"NAME\":\"Joe\",\"AGE\":12}"
*Main> decode "{\"NAME\":\"Joe\",\"AGE\":12}" :: Maybe Person
Just (Person {field_NAME = "Joe", field_AGE = 12})
-}
