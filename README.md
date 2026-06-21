<div align="center">

# wattle

A small parser-combinator library for Haskell, with a JSON parser and printer built on it. Depends only on `base` and `containers`.

![License](https://img.shields.io/badge/LICENSE-MIT-blue?style=for-the-badge)  ![Language](https://img.shields.io/badge/HASKELL-5D4F85?style=for-the-badge&logo=haskell&logoColor=white)  ![Tests](https://img.shields.io/badge/TESTS-42%20CHECKS-brightgreen?style=for-the-badge)

Refer to [LICENSE](LICENSE) for the legal bits.

</div>

## What's in it

- `Wattle.Parser`, a `Parser` type with `Functor`, `Applicative`, `Monad` and `Alternative` instances.
- The usual primitives: `satisfy`, `char`, `string`, `oneOf`, `noneOf`, `digit`, `hexDigit`, `eof`, `try`, `choice`, `option`, `between`, `sepBy`, `sepBy1`, `count` and `chainl1`.
- Labels with `<?>`, and errors that carry a line and column, the things that were expected, and the character that was found. `renderError` turns one into a sentence.
- `Wattle.JSON`, which parses JSON into a `JSON` value and prints it back, compactly with `render` or indented with `pretty`.

```haskell
import Wattle.JSON
import Wattle.Parser (renderError)

main :: IO ()
main = case parseJSON "{\"name\": \"wattle\", \"tags\": [1, 2]}" of
  Left err -> putStrLn (renderError err)
  Right value -> putStrLn (pretty value)
```

Writing your own parser:

```haskell
import Control.Applicative (some)
import Wattle.Parser

number :: Parser Int
number = read <$> some digit

sumP :: Parser Int
sumP = chainl1 number ((+) <$ char '+')
```

`parse sumP "1+2+3"` is `Right 6`.

## 🏗️ Architecture

```
 src/Wattle
  ├─ Parser.hs
  │    State { stateInput, statePos }
  │    Parser a = State -> Either ParseError (a, State)
  │    ├─ instances        Functor / Applicative / Monad / Alternative
  │    ├─ primitives       satisfy, char, string, digit, eof ...
  │    ├─ combinators      try, <?>, choice, option, between, sepBy, count, chainl1
  │    └─ errors           Pos, ParseError { errorPos, errorExpected, errorFound }
  │                        mergeErrors, renderError
  └─ JSON.hs               built only from Parser's exports
       value = null | bool | number | string | array | object
       parseJSON  :: String -> Either ParseError JSON
       render / pretty :: JSON -> String
 test/Spec.hs              list of Check name Bool, plus a seeded generator for round trips
```

- `Parser` is a function from a position-tracking state to either an error or a value with the new state. There's no separate failure-with-consumption flag: a branch has consumed input if its error position differs from where it started.
- Choice follows the Parsec rule. If the left side of `<|>` fails without consuming input, the right side is tried. If it fails after consuming input, that error is returned, and `try` opts back in to backtracking.
- Errors from alternatives that fail at the same position are merged, so `char 'a' <|> char 'b'` reports both expectations.
- The JSON parser handles `\uXXXX` escapes including surrogate pairs, rejects leading zeros and raw control characters in strings, and reports positions on the line they occur.
- Object keys live in a `Map`, so they print in sorted order, and a duplicate key keeps the last value.

## 🚀 Quick start

```
cabal test
```

The test suite uses only `base`: it runs a list of checks and exits with a failure code if any don't hold. The JSON example above prints:

```
{
  "name": "wattle",
  "tags": [
    1,
    2
  ]
}
```

and a malformed document gives a located error, for example `parseJSON "[1 2]"`:

```
line 1, column 4: expected ']', found '2'
```

## 🧪 Testing

42 checks, all passing (`cabal test`; the suite is a plain executable rather than a test framework, so these are checks, not framework test cases):

- Parser (21): `char`, the Functor, Applicative and Monad instances, alternatives, `some` and `many`, `sepBy`, `between`, `chainl1`, `eof`, `runParser` returning the rest, error line and column, `<?>` labels, merged expectations, a committed branch not being retried, `try` allowing backtracking, and `renderError`.
- JSON (16): `null`, booleans, integers, negative fractions, exponents, a leading zero rejected, string escapes, a surrogate pair, a raw control character rejected, empty and nested containers, the position and expectation for a trailing comma, an unterminated string, text after a value, and an error on the second line.
- Printing (5): compact `render`, string escaping, `pretty`, and compact and pretty round trips over 300 pseudo-random JSON values from a seeded generator.

## ⚠️ Honest limits

- It works on `String`, which is simple and slow. It's not meant for large inputs.
- JSON numbers are read as `Double`, so integers beyond 2^53 lose precision.
- An alternative that succeeds discards earlier errors, so a message such as the one for `[1 2]` names only what the last failing parser wanted.

## Licence

MIT.

