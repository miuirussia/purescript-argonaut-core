module Data.Argonaut.Parser (jsonParser) where

import Data.Argonaut.Core (Json(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn9, runFn9)

foreign import _jsonParser :: forall a b c d e f. Fn9 (String -> a) (Json -> a) (b -> Json) (c -> Json) (d -> Json) (e -> Json) (f -> Json) Json String a

-- | Parse a JSON string, constructing the `Json` value described by the string.
-- | To convert a string into a `Json` string, see `fromString`.
jsonParser :: String -> Either String Json
jsonParser j = runFn9 _jsonParser Left Right JNumber JString JArray JObject JBoolean JNull j
