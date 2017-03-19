sum2 :: Num a => [a] -> a
sum2 l = sum2Helper l 0
  where 
    sum2Helper [] a = a
    sum2Helper (x:xs) a = sum2Helper xs (a+x)

sum3 :: Num a => [a] -> a
sum3 l = sum3Helper l 0
  where
    sum3Helper [] a = a
    sum3Helper (x:xs) a = x + sum3Helper xs a
