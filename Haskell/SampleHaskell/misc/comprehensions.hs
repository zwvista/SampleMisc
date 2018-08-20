{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonadComprehensions #-}


import GHC.Exts
import Data.List
import Data.Ord

main1 = print [ (w, x, y, z) | w <- [1 .. 3],
                               x <- [2 .. 4]
                             | y <- [3 .. 5],
                               z <- [4 .. 6] ]
                               
 
main2 = print $ [ (x, y, map the v) | x <- [1 .. 10],
                                      y <- [1 .. 10],
                                      let v = x + y,
                                      then group by v using groupWith,
                                      then take 10,
                                      then group using permutations,
                                      t <- concat v,
                                      then takeWhile by t < 3]
                                     
                                     
parallelListComp :: [Int]
parallelListComp = [ x + y * z
                   | x <- [0..10]
                   | y <- [10..20]
                   | z <- [20..30]
                   ]
                   
parallelListComp2 :: [Int]
parallelListComp2 = zipWith3 (\x y z -> x + y * z) [0..10] [10..20] [20..30]

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs2 :: [Int]
fibs2 = 0 : 1 : [ x + y
                | x <- fibs
                | y <- tail fibs
                ]

fiblikes :: [Int]
fiblikes = 0 : 1 : [ x + y + z
                   | x <- fibs
                   | y <- tail fibs
                   | z <- tail (tail fibs)
                   ]

                                     
data Character = Character
  { firstName :: String
  , lastName :: String
  , birthYear :: Int
  } deriving (Show, Eq)

friends :: [Character]
friends = [ Character "Phoebe" "Buffay" 1963
          , Character "Chandler" "Bing" 1969
          , Character "Rachel" "Green" 1969
          , Character "Joey" "Tribbiani" 1967
          , Character "Monica" "Geller" 1964
          , Character "Ross" "Geller" 1966
          ]

oldest :: Int -> [Character] -> [String]
oldest k tbl = [ firstName ++ " " ++ lastName
               | Character{..} <- tbl
               , then sortWith by birthYear
               , then take k
               ]
               
groupByLargest :: Ord b => (a -> b) -> [a] -> [[a]]
groupByLargest f = sortBy (comparing (negate . length)) . groupWith f

bestBirthYears :: [Character] -> [(Int, [String])]
bestBirthYears tbl = [ (the birthYear, firstName)
                     | Character{..} <- tbl
                     , then group by birthYear using groupByLargest
                     ]

main3 = do
    print $ oldest 2 friends
    print $ bestBirthYears friends
    

l :: [(String, Int)]
l = [("a", 1), ("b", 2), ("c", 3)]

main4 = print [ (x, y) | x <- lookup "a" l,
                         y <- lookup "b" l,
                         then (\f ->
                                maybe Nothing
                                      (\x -> if f x == 2
                                                then Just x
                                                else Nothing))
                              by (x * y) ]

