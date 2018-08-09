import GetURL
import TimeIt

import Control.Concurrent.Async
import Text.Printf
import qualified Data.ByteString as B

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main = do
 as <- mapM (async . timeDownload) sites
 mapM_ wait as
