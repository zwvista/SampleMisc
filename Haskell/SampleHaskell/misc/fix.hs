import Data.Function

map2 :: (a -> b) -> [a] -> [b]
map2 = fix $ \rec f list ->
    case list of
        [] -> []
        (x:xs) -> f x : rec f xs

map3 :: (a -> b) -> [a] -> [b]
map3 f = fix $ \rec list ->
    case list of
        [] -> []
        (x:xs) -> f x : rec xs

cosFixpointExplicit x =
    if cos x == x
       then x
       else cosFixpointExplicit (cos x)

cosFixpoint x =
    fix (\f b -> 
            if cos b == b 
               then b
               else f (cos b)
         ) x

cosFixpoint2 x =
    ($ x) . fix $ \f b -> 
            if cos b == b 
               then b
               else f (cos b)

cosFixpoint3 x =
    flip fix x $ \f b -> 
            if cos b == b 
               then b
               else f (cos b)
