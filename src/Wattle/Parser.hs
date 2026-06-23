module Wattle.Parser
  ( Parser
  , Pos (..)
  , ParseError (..)
  , parse
  , runParser
  , renderError
  , satisfy
  , char
  , string
  , anyChar
  , oneOf
  , noneOf
  , digit
  , hexDigit
  , eof
  , try
  , (<?>)
  , skipMany
  , choice
  , option
  , between
  , sepBy
  , sepBy1
  , count
  , chainl1
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (replicateM)
import Data.Char (isDigit, isHexDigit)
import Data.List (intercalate, nub)

data Pos = Pos
  { posLine :: !Int
  , posColumn :: !Int
  }
  deriving (Eq, Ord, Show)

data ParseError = ParseError
  { errorPos :: Pos
  , errorExpected :: [String]
  , errorFound :: Maybe Char
  }
  deriving (Eq, Show)

data State = State
  { stateInput :: String
  , statePos :: !Pos
  }

newtype Parser a = Parser {unParser :: State -> Either ParseError (a, State)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Left e -> Left e
    Right (a, s') -> Right (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)
  pf <*> pa = pf >>= \f -> fmap f pa

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> case p s of
    Left e -> Left e
    Right (a, s') -> unParser (f a) s'

instance Alternative Parser where
  empty = Parser $ \s -> Left (ParseError (statePos s) [] (peek s))
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Left e
      | errorPos e == statePos s -> case q s of
          Left e' -> Left (mergeErrors e e')
          r -> r
    r -> r

peek :: State -> Maybe Char
peek s = case stateInput s of
  (c : _) -> Just c
  [] -> Nothing

mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors a b = case compare (errorPos a) (errorPos b) of
  GT -> a
  LT -> b
  EQ ->
    a
      { errorExpected = nub (errorExpected a ++ errorExpected b)
      , errorFound = maybe (errorFound b) Just (errorFound a)
      }

advance :: Char -> Pos -> Pos
advance '\n' (Pos l _) = Pos (l + 1) 1
advance _ (Pos l c) = Pos l (c + 1)

startState :: String -> State
startState input = State input (Pos 1 1)

parse :: Parser a -> String -> Either ParseError a
parse p input = fst <$> unParser (p <* eof) (startState input)

runParser :: Parser a -> String -> Either ParseError (a, String)
runParser p input = case unParser p (startState input) of
  Left e -> Left e
  Right (a, s) -> Right (a, stateInput s)

renderError :: ParseError -> String
renderError (ParseError (Pos l c) expected found) =
  "line " ++ show l ++ ", column " ++ show c ++ ": " ++ wanted ++ ", found " ++ got
  where
    wanted = case expected of
      [] -> "unexpected input"
      _ -> "expected " ++ listOr expected
    got = maybe "end of input" show found
    listOr [x] = x
    listOr xs = intercalate ", " (init xs) ++ " or " ++ last xs

satisfy :: (Char -> Bool) -> Parser Char
satisfy ok = Parser $ \s -> case stateInput s of
  (c : cs) | ok c -> Right (c, State cs (advance c (statePos s)))
  _ -> Left (ParseError (statePos s) [] (peek s))

char :: Char -> Parser Char
char c = satisfy (== c) <?> show c

string :: String -> Parser String
string str = mapM_ char str *> pure str <?> show str

anyChar :: Parser Char
anyChar = satisfy (const True) <?> "any character"

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs) <?> ("one of " ++ show cs)

noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs) <?> ("none of " ++ show cs)

digit :: Parser Char
digit = satisfy isDigit <?> "digit"

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"

eof :: Parser ()
eof = Parser $ \s -> case stateInput s of
  [] -> Right ((), s)
  (c : _) -> Left (ParseError (statePos s) ["end of input"] (Just c))

try :: Parser a -> Parser a
try (Parser p) = Parser $ \s -> case p s of
  Left e -> Left e {errorPos = statePos s}
  r -> r

infix 0 <?>

(<?>) :: Parser a -> String -> Parser a
Parser p <?> name = Parser $ \s -> case p s of
  Left e | errorPos e == statePos s -> Left e {errorExpected = [name]}
  r -> r

skipMany :: Parser a -> Parser ()
skipMany p = () <$ many p

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

option :: a -> Parser a -> Parser a
option def p = p <|> pure def

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

count :: Int -> Parser a -> Parser [a]
count = replicateM

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest acc = (do f <- op; x <- p; rest (f acc x)) <|> pure acc

