-- helpers.hs

import Data.Map (fromListWith)

sortAndGroup kv = fromListWith (++) [(k, [v]) | (k, v) <- kv]
sortAndGroupBy f xs = fromListWith (++) [(f x, [x]) | x <- xs]

