add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
    square_cps x $ \x_squared ->
    square_cps y $ \y_squared ->
    add_cps x_squared y_squared $ k
    
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS s f = \k -> s $ \x -> f x $ k

pythagoras_cps' :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps' x y =
    square_cps x `chainCPS` \x_squared ->
    square_cps y `chainCPS` \y_squared ->
    add_cps x_squared y_squared

main = do
    print $ pythagoras 3 4
    pythagoras_cps 3 4 print
    pythagoras_cps' 3 4 print

{-
25
25
25
-}
