import GetURL

import Control.Concurrent.Async
import qualified Data.ByteString as B

main1 = do
  (r1, r2) <- concurrently 
    (getURL "http://www.wikipedia.org/wiki/Shovel") 
    (getURL "http://www.wikipedia.org/wiki/Spade")
  print (B.length r1, B.length r2)

main2 = do
  [r1, r2] <- mapConcurrently getURL
    [ "http://www.wikipedia.org/wiki/Shovel"
    , "http://www.wikipedia.org/wiki/Spade"]
  print (B.length r1, B.length r2)

main3 = do
  (r1, r2) <- runConcurrently $ (,)
    <$> (Concurrently (getURL "http://www.wikipedia.org/wiki/Shovel"))
    <*> (Concurrently (getURL "http://www.wikipedia.org/wiki/Spade"))
  print (B.length r1, B.length r2)

main4 = do
  r <- race 
    (getURL "http://www.wikipedia.org/wiki/Shovel") 
    (getURL "http://www.wikipedia.org/wiki/Spade")
  print (B.length <$> r)
