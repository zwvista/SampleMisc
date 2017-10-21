import Control.Monad.Trans.Cont
import Control.Monad (when)

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (add x y)

square_cont :: Int -> Cont r Int
square_cont x = return (square x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
    x_squared <- square_cont x
    y_squared <- square_cont y
    add_cont x_squared y_squared

pythagoras_cont' :: Int -> Int -> Cont r Int
pythagoras_cont' x y = callCC $ \exit -> do
    when (x < 0 || y < 0) $ exit (-1)
    x_squared <- square_cont x
    y_squared <- square_cont y
    add_cont x_squared y_squared

main = do
    runCont (pythagoras_cont 3 4) print -- 25
    runCont (pythagoras_cont' 3 4) print -- 25
    runCont (pythagoras_cont' (-3) 4) print -- -1
