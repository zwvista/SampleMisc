import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>))

number = many1 digit
plus = char '+' *> number
minus = (:) <$> char '-' <*> number
integer = plus <|> minus <|> number
float = fmap rd $ (++) <$> integer <*> decimal
    where rd      = read :: String -> Float
          decimal = option "" $ (:) <$> char '.' <*> number

main = forever $ do putStrLn "Enter a float: "
                    input <- getLine
                    parseTest float input
