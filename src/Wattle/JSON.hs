module Wattle.JSON
  ( JSON (..)
  , json
  , parseJSON
  , render
  , pretty
  ) where

import Control.Applicative (many, some, (<|>))
import Data.Char (chr, ord, digitToInt)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Numeric (showHex)
import Wattle.Parser

data JSON
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JSON]
  | JObject (Map.Map String JSON)
  deriving (Eq, Show)

parseJSON :: String -> Either ParseError JSON
parseJSON = parse json

json :: Parser JSON
json = ws *> value

ws :: Parser ()
ws = skipMany (oneOf " \t\n\r")

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

symbol :: Char -> Parser Char
symbol = lexeme . char

value :: Parser JSON
value =
  lexeme (choice [nullP, boolP, numberP, JString <$> stringLit, arrayP, objectP])
    <?> "JSON value"

nullP :: Parser JSON
nullP = JNull <$ string "null"

boolP :: Parser JSON
boolP = JBool True <$ string "true" <|> JBool False <$ string "false"

numberP :: Parser JSON
numberP = do
  sign <- option "" (string "-")
  whole <- string "0" <|> ((:) <$> oneOf "123456789" <*> many digit)
  frac <- option "" ((:) <$> char '.' <*> some digit)
  expo <- option "" exponentP
  pure (JNumber (read (sign ++ whole ++ frac ++ expo)))

exponentP :: Parser String
exponentP = do
  e <- oneOf "eE"
  sign <- option "" (pure <$> oneOf "+-")
  ds <- some digit
  pure (e : sign ++ ds)

stringLit :: Parser String
stringLit = char '"' *> (combine <$> many unit) <* char '"'

unit :: Parser Int
unit =
  (ord <$> satisfy (\c -> c /= '"' && c /= '\\' && c >= ' '))
    <|> (char '\\' *> escape)

escape :: Parser Int
escape =
  choice
    [ 34 <$ char '"'
    , 92 <$ char '\\'
    , 47 <$ char '/'
    , 8 <$ char 'b'
    , 12 <$ char 'f'
    , 10 <$ char 'n'
    , 13 <$ char 'r'
    , 9 <$ char 't'
    , char 'u' *> (foldl (\acc d -> acc * 16 + digitToInt d) 0 <$> count 4 hexDigit)
    ]
    <?> "escape sequence"

combine :: [Int] -> String
combine (hi : lo : rest)
  | isHigh hi && isLow lo = chr (0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00)) : combine rest
combine (u : rest)
  | isHigh u || isLow u = '\xFFFD' : combine rest
  | otherwise = chr u : combine rest
combine [] = []

isHigh, isLow :: Int -> Bool
isHigh u = u >= 0xD800 && u <= 0xDBFF
isLow u = u >= 0xDC00 && u <= 0xDFFF

arrayP :: Parser JSON
arrayP = JArray <$> between (symbol '[') (char ']') (sepBy value (symbol ','))

objectP :: Parser JSON
objectP = JObject . Map.fromList <$> between (symbol '{') (char '}') (sepBy member (symbol ','))
  where
    member = (,) <$> (lexeme stringLit <?> "string key") <* symbol ':' <*> value

render :: JSON -> String
render JNull = "null"
render (JBool b) = if b then "true" else "false"
render (JNumber n) = renderNumber n
render (JString s) = quote s
render (JArray xs) = "[" ++ intercalate "," (map render xs) ++ "]"
render (JObject m) =
  "{" ++ intercalate "," [quote k ++ ":" ++ render v | (k, v) <- Map.toList m] ++ "}"

pretty :: JSON -> String
pretty = go 0
  where
    go _ (JArray []) = "[]"
    go lvl (JArray xs) =
      "[\n" ++ intercalate ",\n" [indent (lvl + 1) ++ go (lvl + 1) x | x <- xs] ++ "\n" ++ indent lvl ++ "]"
    go _ (JObject m) | Map.null m = "{}"
    go lvl (JObject m) =
      "{\n"
        ++ intercalate ",\n" [indent (lvl + 1) ++ quote k ++ ": " ++ go (lvl + 1) v | (k, v) <- Map.toList m]
        ++ "\n"
        ++ indent lvl
        ++ "}"
    go _ other = render other
    indent lvl = replicate (2 * lvl) ' '

renderNumber :: Double -> String
renderNumber n
  | isNaN n || isInfinite n = "null"
  | n == fromInteger whole && abs n < 1e15 = show whole
  | otherwise = show n
  where
    whole = round n :: Integer

quote :: String -> String
quote s = "\"" ++ concatMap escapeChar s ++ "\""

escapeChar :: Char -> String
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar '\b' = "\\b"
escapeChar '\f' = "\\f"
escapeChar c
  | c < ' ' = "\\u" ++ replicate (4 - length hex) '0' ++ hex
  | otherwise = [c]
  where
    hex = showHex (ord c) ""
