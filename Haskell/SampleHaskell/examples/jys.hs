-- jys.hs

import Data.Map.Strict (fromListWith, elems)
import Data.List (intercalate, groupBy, sortOn)
import Data.Function (on)

verticalWriting text offset =
    let indexedChars = zipWith (\x y -> (x `mod` offset, [y])) [0..] text
    in elems $ fromListWith (\a b -> a ++ "|" ++ b) indexedChars
    -- in map (intercalate "|" . reverse . map snd) $ groupBy ((==) `on` fst) $ sortOn fst $ indexedChars

main = mapM_ putStrLn $ verticalWriting "床前明月光疑是地上霜举头望明月低头思故乡" 5

{-
低|举|疑|床
头|头|是|前
思|望|地|明
故|明|上|月
乡|月|霜|光
-}

