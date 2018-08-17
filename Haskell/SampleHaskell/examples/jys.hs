-- jys.hs

import Data.Map.Strict (fromListWith, elems)
import Data.List (intersperse, groupBy, sortOn)
import Data.Function (on)
import GHC.Exts (groupWith)

verticalWriting text offset =
    let indexedChars = zipWith (\x y -> (x `mod` offset, [y])) [0..] text
    in elems $ fromListWith (\a b -> a ++ "|" ++ b) indexedChars
    
verticalWriting2 text offset =
    let indexedChars = zipWith (\x y -> (x `mod` offset, y)) [0..] text
    in intersperse '|' . reverse . map snd <$> groupBy ((==) `on` fst) (sortOn fst indexedChars)
    
verticalWriting3 text offset =
    let indexedChars = zipWith (\x y -> (x `mod` offset, y)) [0..] text
    in intersperse '|' . reverse . snd . unzip <$> groupWith fst indexedChars

main = do
    mapM_ putStrLn $ verticalWriting "床前明月光疑是地上霜举头望明月低头思故乡" 5
    mapM_ putStrLn $ verticalWriting2 "床前明月光疑是地上霜举头望明月低头思故乡" 5
    mapM_ putStrLn $ verticalWriting3 "床前明月光疑是地上霜举头望明月低头思故乡" 5


{-
低|举|疑|床
头|头|是|前
思|望|地|明
故|明|上|月
乡|月|霜|光
-}

