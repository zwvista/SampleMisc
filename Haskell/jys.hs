import Data.Map

(|>) x f = f x
sortAndGroup kv = fromListWith (++) [(k, [v]) | (k, v) <- kv]
sortAndGroupBy f a = sortAndGroup $ zip (Prelude.map f a) a

print :: String -> int -> String
print text offset = undefined