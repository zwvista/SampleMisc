-- jys2.hs

import Data.Map (fromListWith, toAscList)
import Data.List (intersperse)
import Data.Function ((&))

sortAndGroupBy f xs = fromListWith (++) [(f x, [x]) | x <- xs]

verticalWriting text offset = zip [0..] text
    & sortAndGroupBy ((`mod` offset) . fst)
    & toAscList
    & map snd
    & map (map snd)
    & map (intersperse '|')
    & unlines
    
main = putStr $ verticalWriting "床前明月光疑是地上霜举头望明月低头思故乡" 5

--低|举|疑|床
--头|头|是|前
--思|望|地|明
--故|明|上|月
--乡|月|霜|光
