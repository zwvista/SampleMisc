-- cabal install contravariant

import Data.Functor.Contravariant

greaterThanThree :: Predicate Int
greaterThanThree = Predicate (> 3)

lengthGTThree :: Predicate [a]
lengthGTThree = contramap length greaterThanThree

englishGTThree :: Predicate Int
englishGTThree = contramap english lengthGTThree

english :: Int -> String
english 1 = "one"
english 2 = "two"
english 3 = "three"
english 4 = "four"
english 5 = "five"
english 6 = "six"
english 7 = "seven"
english 8 = "eight"
english 9 = "nine"
english 10 = "ten"

main :: IO ()
main = print $ filter (getPredicate englishGTThree) [1..10]

-- [3,4,5,7,8,9]
