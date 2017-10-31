import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop  

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  

main = do
    print $ runState stackManip [5,8,2,1]
    print $ runState stackStuff [9,0,2,1,0]
    print $ runState stackyStack [9,0,2,1,0]
    
{-
(5,[8,2,1])
((),[8,3,0,2,1,0])
((),[9,2,1])
-}
