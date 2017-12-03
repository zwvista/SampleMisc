import Text.Parsec
import Control.Applicative ((<$), (<*), (*>), liftA)
import Data.Char           (chr)

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvp ""

csvp :: Parsec String () [[String]]
csvp = line `endBy` newline <* eof

line :: Parsec String () [String]
line = cell `sepBy1` char ','

cell :: Parsec String () String
cell = cell' <|> many (noneOf ",\n")
    where cell' = between (char '"') (char '"') $ many chr
          chr   = noneOf "\"" <|> try ('"' <$ string "\"\"")

