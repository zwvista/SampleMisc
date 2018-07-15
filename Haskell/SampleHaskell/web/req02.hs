{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

main1 :: IO ()
main1 = runReq def $ do
  bs <- req GET (https "jsonplaceholder.typicode.com" /: "posts" /: "1") NoReqBody bsResponse mempty
  liftIO $ B.putStrLn (responseBody bs)

data Post = Post
  { userId  :: Int
  , id :: Int
  , title :: Text
  , body :: Text
  } deriving (Show, Generic)

instance ToJSON Post
instance FromJSON Post

main2 :: IO ()
main2 = runReq def $ do
  v <- req GET (https "jsonplaceholder.typicode.com" /: "posts" /: "1") NoReqBody jsonResponse mempty
  liftIO $ print (responseBody v :: Post)

main3 :: IO ()
main3 = runReq def $ do
  v <- req GET (https "jsonplaceholder.typicode.com" /: "posts") NoReqBody jsonResponse mempty
  liftIO $ print (responseBody v :: [Post])

main4 :: IO ()
main4 = runReq def $ do
  let myPost = Post {
    userId = 101,
    Main.id = 102,
    title = "test title",
    body = "test body"
    }
  v <- req POST (https "jsonplaceholder.typicode.com" /: "posts") (ReqBodyJson myPost) jsonResponse mempty
  liftIO $ print (responseBody v :: Value)

main5 :: IO ()
main5 = runReq def $ do
  let myPost = Post {
    userId = 101,
    Main.id = 102,
    title = "test title",
    body = "test body"
    }
  v <- req PUT (https "jsonplaceholder.typicode.com" /: "posts" /: "1") (ReqBodyJson myPost) jsonResponse mempty
  liftIO $ print (responseBody v :: Value)

main6 :: IO ()
main6 = runReq def $ do
  v <- req DELETE (https "jsonplaceholder.typicode.com" /: "posts" /: "1") NoReqBody jsonResponse mempty
  liftIO $ print (responseBody v :: Value)
