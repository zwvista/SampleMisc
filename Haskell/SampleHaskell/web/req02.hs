{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

showPostAsString :: Int -> IO ()
showPostAsString n = runReq def $ do
  bs <- req GET (https "jsonplaceholder.typicode.com" /: "posts" /~ n) NoReqBody bsResponse mempty
  liftIO $ B.putStrLn (responseBody bs)

data Post = Post
  { userId  :: Int
  , id :: Int
  , title :: Text
  , body :: Text
  } deriving (Show, Generic)

instance ToJSON Post
instance FromJSON Post

showPostAsJson :: Int -> IO ()
showPostAsJson n = runReq def $ do
  v <- req GET (https "jsonplaceholder.typicode.com" /: "posts" /~ n) NoReqBody jsonResponse mempty
  liftIO $ print (responseBody v :: Post)

showPosts :: Int -> IO ()
showPosts n = runReq def $ do
  v <- req GET (https "jsonplaceholder.typicode.com" /: "posts") NoReqBody jsonResponse mempty
  liftIO $ mapM_ print $ take n (responseBody v :: [Post])

createPost :: IO ()
createPost = runReq def $ do
  let myPost = Post {
    userId = 101,
    Main.id = 102,
    title = "test title",
    body = "test body"
    }
  v <- req POST (https "jsonplaceholder.typicode.com" /: "posts") (ReqBodyJson myPost) jsonResponse mempty
  liftIO $ print (responseBody v :: Post)

updatePost :: Int -> IO ()
updatePost n = runReq def $ do
  let myPost = Post {
    userId = 101,
    Main.id = 0, -- unused
    title = "test title",
    body = "test body"
    }
  v <- req PUT (https "jsonplaceholder.typicode.com" /: "posts" /~ n) (ReqBodyJson myPost) jsonResponse mempty
  liftIO $ print (responseBody v :: Post)

deletePost :: Int -> IO ()
deletePost n = runReq def $ do
  v <- req DELETE (https "jsonplaceholder.typicode.com" /: "posts" /~ n) NoReqBody jsonResponse mempty
  liftIO $ print (responseBody v :: Value)

{-
*Main> showPostAsString 1
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
}
*Main> showPostAsJson 1
Post {userId = 1, id = 1, title = "sunt aut facere repellat provident occaecati excepturi optio reprehenderit", body = "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"}
*Main> showPosts 2
Post {userId = 1, id = 1, title = "sunt aut facere repellat provident occaecati excepturi optio reprehenderit", body = "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"}
Post {userId = 1, id = 2, title = "qui est esse", body = "est rerum tempore vitae\nsequi sint nihil reprehenderit dolor beatae ea dolores neque\nfugiat blanditiis voluptate porro vel nihil molestiae ut reiciendis\nqui aperiam non debitis possimus qui neque nisi nulla"}
*Main> createPost
Post {userId = 101, id = 102, title = "test title", body = "test body"}
*Main> updatePost 1
Post {userId = 101, id = 1, title = "test title", body = "test body"}
*Main> deletePost 1
Object (fromList [])
-}