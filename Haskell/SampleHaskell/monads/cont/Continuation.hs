module Continuation where

idCPS :: a -> (a -> r) -> r
idCPS a ret = ret a

mysqrt :: Floating a => a -> a
mysqrt x = sqrt x

test1 :: IO ()
test1 = print (mysqrt 4)

mysqrtCPS :: Floating a => a -> (a -> r) -> r
mysqrtCPS a k = k (sqrt a)

test2 :: IO ()
test2 = mysqrtCPS 4 print

test3 :: Floating a => a
test3 = mysqrt 4 + 2

test4 :: Floating a => a
test4= mysqrtCPS 4 (+ 2)

fac :: Integral a => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

test5 :: Integral a => a
test5 = fac 4 + 2

facCPS :: Integral a => a -> (a -> r) -> r
facCPS 0 k = k 1
facCPS n k = facCPS (n - 1) $ \ret -> k (n * ret)

test6 :: Integral a => a
test6 = facCPS 4 (+ 2)

test7 :: IO ()
test7 = facCPS 4 print

newSentence :: Char -> Bool
newSentence = flip elem ".?!"

newSentenceCPS :: Char -> (Bool -> r) -> r
newSentenceCPS c k = k (elem c ".?!")

mylength :: [a] -> Integer
mylength [] = 0
mylength (_ : as) = succ (mylength as)

mylengthCPS :: [a] -> (Integer -> r) -> r
mylengthCPS [] k = k 0
mylengthCPS (_ : as) k = mylengthCPS as (k . succ)

test8 :: Integer
test8 = mylengthCPS [1..2006] id

test9 :: IO ()
test9 = mylengthCPS [1..2006] print

main = do
    test1
    test2
    print test3
    print test4
    print test5
    print test6
    test7
    print test8
    test9
    
{-
2.0
2.0
4.0
4.0
26
26
24
2006
2006
-}
