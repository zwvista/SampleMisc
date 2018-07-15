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
{-
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
}
-}

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
-- Post {userId = 1, id = 1, title = "sunt aut facere repellat provident occaecati excepturi optio reprehenderit", body = "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"}

main3 :: IO ()
main3 = runReq def $ do
  v <- req GET (https "jsonplaceholder.typicode.com" /: "posts") NoReqBody jsonResponse mempty
  liftIO $ mapM_ print $ take 2 (responseBody v :: [Post])
{-
Post {userId = 1, id = 1, title = "sunt aut facere repellat provident occaecati excepturi optio reprehenderit", body = "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"}
Post {userId = 1, id = 2, title = "qui est esse", body = "est rerum tempore vitae\nsequi sint nihil reprehenderit dolor beatae ea dolores neque\nfugiat blanditiis voluptate porro vel nihil molestiae ut reiciendis\nqui aperiam non debitis possimus qui neque nisi nulla"}
-}

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
-- Object (fromList [("body",String "test body"),("userId",Number 101.0),("id",Number 1.0),("title",String "test title")])

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
-- Object (fromList [("body",String "test body"),("userId",Number 101.0),("id",Number 1.0),("title",String "test title")])

main6 :: IO ()
main6 = runReq def $ do
  v <- req DELETE (https "jsonplaceholder.typicode.com" /: "posts" /: "2") NoReqBody jsonResponse mempty
  liftIO $ print (responseBody v :: Value)
-- Object (fromList [])
