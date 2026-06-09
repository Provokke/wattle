module Main (main) where

import Control.Applicative ((<|>), many, some)
import Control.Monad (unless)
import Data.Char (digitToInt)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Wattle.JSON
import Wattle.Parser

data Check = Check String Bool

errPos :: Either ParseError a -> Maybe Pos
errPos = either (Just . errorPos) (const Nothing)

errExpected :: Either ParseError a -> [String]
errExpected = either errorExpected (const [])

isLeft' :: Either a b -> Bool
isLeft' = either (const True) (const False)

parserChecks :: [Check]
parserChecks =
  [ Check "char" (parse (char 'a') "a" == Right 'a')
  , Check "functor" (parse (succ <$> char 'a') "a" == Right 'b')
  , Check "applicative" (parse ((,) <$> char 'a' <*> char 'b') "ab" == Right ('a', 'b'))
  , Check "monad" (parse (digit >>= \d -> count (digitToInt d) (char 'x')) "2xx" == Right "xx")
  , Check "alternative picks second" (parse (string "ab" <|> string "cd") "cd" == Right "cd")
  , Check "some" (parse (some digit) "123" == Right "123")
  , Check "some needs one" (isLeft' (parse (some digit) "abc"))
  , Check "many accepts none" (parse (many digit) "" == Right "")
  , Check "sepBy" (parse (sepBy digit (char ',')) "1,2,3" == Right "123")
  , Check "sepBy empty" (parse (sepBy digit (char ',')) "" == Right "")
  , Check "between" (parse (between (char '(') (char ')') digit) "(7)" == Right '7')
  , Check "chainl1" (parse (chainl1 (digitToInt <$> digit) ((-) <$ char '-')) "9-3-2" == Right 4)
  , Check "eof rejects leftovers" (isLeft' (parse (char 'a') "ab"))
  , Check "runParser returns the rest" (runParser (char 'a') "abc" == Right ('a', "bc"))
  , Check "error column" (errPos (parse (string "ab") "ax") == Just (Pos 1 2))
  , Check "error line" (errPos (parse (char 'a' *> char '\n' *> char 'b') "a\nc") == Just (Pos 2 1))
  , Check "label" (errExpected (parse (char 'a' <?> "letter a") "z") == ["letter a"])
  , Check "alternatives merge expectations" (errExpected (parse (char 'a' <|> char 'b') "z") == ["'a'", "'b'"])
  , Check "committed branch is not retried" (isLeft' (parse (string "ab" <|> string "ac") "ac"))
  , Check "try allows backtracking" (parse (try (string "ab") <|> string "ac") "ac" == Right "ac")
  , Check "render error" (fmap renderError (either Just (const Nothing) (parse (char 'a') "")) == Just "line 1, column 1: expected 'a', found end of input")
  ]

jsonChecks :: [Check]
jsonChecks =
  [ Check "null" (parseJSON "null" == Right JNull)
  , Check "bools" (parseJSON " true " == Right (JBool True) && parseJSON "false" == Right (JBool False))
  , Check "integer" (parseJSON "42" == Right (JNumber 42))
  , Check "negative fraction" (parseJSON "-0.5" == Right (JNumber (-0.5)))
  , Check "exponent" (parseJSON "1.5e3" == Right (JNumber 1500))
  , Check "leading zero rejected" (isLeft' (parseJSON "01"))
  , Check "string escapes" (parseJSON "\"a\\n\\t\\\"\\u0041\"" == Right (JString "a\n\t\"A"))
  , Check "surrogate pair" (parseJSON "\"\\ud83d\\ude00\"" == Right (JString "\x1F600"))
  , Check "raw control char rejected" (isLeft' (parseJSON "\"a\nb\""))
  , Check "empty containers" (parseJSON "[ ]" == Right (JArray []) && parseJSON "{ }" == Right (JObject Map.empty))
  , Check "nested" (parseJSON "{\"a\": [1, {\"b\": null}], \"c\": \"d\"}" == Right (JObject (Map.fromList [("a", JArray [JNumber 1, JObject (Map.fromList [("b", JNull)])]), ("c", JString "d")])))
  , Check "trailing comma position" (errPos (parseJSON "[1, 2,]") == Just (Pos 1 7))
  , Check "trailing comma expectation" ("JSON value" `elem` errExpected (parseJSON "[1, 2,]"))
  , Check "unterminated string" (isLeft' (parseJSON "\"abc"))
  , Check "garbage after value" (errPos (parseJSON "1 2") == Just (Pos 1 3))
  , Check "error on second line" (errPos (parseJSON "{\n  \"a\" 1\n}") == Just (Pos 2 7))
  , Check "render compact" (render (JObject (Map.fromList [("k", JArray [JNumber 1, JNumber 2.5, JBool True])])) == "{\"k\":[1,2.5,true]}")
  , Check "render escapes" (render (JString "a\"\\\n\1") == "\"a\\\"\\\\\\n\\u0001\"")
  , Check "pretty" (pretty (JObject (Map.fromList [("a", JArray [JNumber 1]), ("b", JArray [])])) == "{\n  \"a\": [\n    1\n  ],\n  \"b\": []\n}")
  , Check "round trip, compact" (all (\v -> parseJSON (render v) == Right v) samples)
  , Check "round trip, pretty" (all (\v -> parseJSON (pretty v) == Right v) samples)
  ]

next :: Int -> Int
next s = (s * 1103515245 + 12345) `mod` 2147483648

draw :: Int -> Int -> Int
draw s m = (s `div` 65536) `mod` m

genString :: Int -> (String, Int)
genString s0 = foldl step ([], s1) [1 .. draw s1 6]
  where
    s1 = next s0
    palette = "ab z\"\\\n\t/\233"
    step (acc, s) _ =
      let s' = next s
       in (acc ++ [palette !! draw s' (length palette)], s')

gen :: Int -> Int -> (JSON, Int)
gen depth s0 = case draw s1 (if depth <= 0 then 4 else 6) of
  0 -> (JNull, s1)
  1 -> (JBool (even (draw s1 100)), s1)
  2 -> (JNumber (fromIntegral (draw s1 2000 - 1000) / 4), s1)
  3 -> let (str, s2) = genString s1 in (JString str, s2)
  4 -> let (xs, s2) = genList depth s1 in (JArray xs, s2)
  _ -> let (kvs, s2) = genPairs depth s1 in (JObject (Map.fromList kvs), s2)
  where
    s1 = next s0

genList :: Int -> Int -> ([JSON], Int)
genList depth s0 = foldl step ([], s1) [1 .. draw s1 4]
  where
    s1 = next s0
    step (acc, s) _ = let (v, s') = gen (depth - 1) s in (acc ++ [v], s')

genPairs :: Int -> Int -> ([(String, JSON)], Int)
genPairs depth s0 = foldl step ([], s1) [1 .. draw s1 4]
  where
    s1 = next s0
    step (acc, s) _ =
      let (k, s') = genString s
          (v, s'') = gen (depth - 1) s'
       in (acc ++ [(k, v)], s'')

samples :: [JSON]
samples = [fst (gen 3 seed) | seed <- [1 .. 300]]

main :: IO ()
main = do
  let checks = parserChecks ++ jsonChecks
      failures = [name | Check name False <- checks]
  mapM_ (putStrLn . ("FAIL: " ++)) failures
  putStrLn (show (length checks - length failures) ++ " of " ++ show (length checks) ++ " checks passed")
  unless (null failures) exitFailure

