{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

main1 :: IO ()
main1 = runReq def $ do
  let n :: Int
      n = 5
  bs <- req GET (https "httpbin.org" /: "bytes" /~ n) NoReqBody bsResponse mempty
  liftIO $ B.putStrLn (responseBody bs)

main2 :: IO ()
main2 = runReq def $ do
  let n, seed :: Int
      n    = 5
      seed = 100
  bs <- req GET (https "httpbin.org" /: "bytes" /~ n) NoReqBody bsResponse $
    "seed" =: seed
  liftIO $ B.putStrLn (responseBody bs)
  
data MyData = MyData
  { size  :: Int
  , color :: Text
  } deriving (Show, Generic)

instance ToJSON MyData
instance FromJSON MyData

main3 :: IO ()
main3 = runReq def $ do
  let myData = MyData
        { size  = 6
        , color = "Green" }
  v <- req POST (https "httpbin.org" /: "post") (ReqBodyJson myData) jsonResponse mempty
  liftIO $ print (responseBody v :: Value)
  
main4 :: IO ()
main4 = runReq def $ do
  let params =
        "foo" =: ("bar" :: Text) <>
        queryFlag "baz"
  response <- req POST (https "httpbin.org" /: "post") (ReqBodyUrlEnc params) jsonResponse mempty
  liftIO $ print (responseBody response :: Value)

main5 :: IO ()
main5 = runReq def $ do
  -- This is an example of what to do when URL is given dynamically. Of
  -- course in a real application you may not want to use 'fromJust'.
  let (url, options) = fromJust (parseUrlHttps "https://httpbin.org/get?foo=bar")
  response <- req GET url NoReqBody jsonResponse $
    "from" =: (15 :: Int)           <>
    "to"   =: (67 :: Int)           <>
    basicAuth "username" "password" <>
    options                         <> -- contains the ?foo=bar part
    port 443 -- here you can put any port of course
  liftIO $ print (responseBody response :: Value)
