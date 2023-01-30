-- | This module defines a data type and various functions for creating and
-- | manipulating JSON values. The README contains additional documentation
-- | for this module.
module Data.Argonaut.Core
  ( Json(..)
  , caseJson
  , caseJsonNull
  , caseJsonBoolean
  , caseJsonNumber
  , caseJsonString
  , caseJsonArray
  , caseJsonObject
  , isNull
  , isBoolean
  , isNumber
  , isString
  , isArray
  , isObject
  , fromBoolean
  , fromNumber
  , fromString
  , fromArray
  , fromObject
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject
  , jsonNull
  , jsonTrue
  , jsonFalse
  , jsonZero
  , jsonEmptyString
  , jsonEmptyArray
  , jsonSingletonArray
  , jsonEmptyObject
  , jsonSingletonObject
  , stringify
  , stringifyWithIndent
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Number.Format (toString) as Number
import Data.String.Common (joinWith) as String
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Obj

-- | The type of JSON data. The underlying representation is the same as what
-- | would be returned from JavaScript's `JSON.parse` function; that is,
-- | ordinary JavaScript booleans, strings, arrays, objects, etc.
data Json
  = JObject (Object Json)
  | JArray (Array Json)
  | JString String
  | JNumber String
  | JBoolean Boolean
  | JNull

instance eqJson :: Eq Json where
  eq JNull JNull = true
  eq j1 j2 = compare j1 j2 == EQ

instance ordJson :: Ord Json where
  compare JNull JNull = EQ
  compare JNull _ = LT
  compare (JBoolean a) (JBoolean b)
    | a == b = EQ
    | a == false = LT
    | otherwise = GT
  compare (JBoolean _) JNull = GT
  compare (JBoolean _) _ = LT
  compare (JNumber a) (JNumber b) = compare a b
  compare (JNumber _) JNull = GT
  compare (JNumber _) (JBoolean _) = GT
  compare (JNumber _) _ = LT
  compare (JString a) (JString b) = compare a b
  compare (JString _) JNull = GT
  compare (JString _) (JBoolean _) = GT
  compare (JString _) (JNumber _) = GT
  compare (JString _) _ = LT
  compare (JArray a) (JArray b) = compare a b
  compare (JArray _) JNull = GT
  compare (JArray _) (JBoolean _) = GT
  compare (JArray _) (JNumber _) = GT
  compare (JArray _) (JString _) = GT
  compare (JArray _) _ = LT
  compare (JObject a) (JObject b) = compare a b
  compare (JObject _) JNull = GT
  compare (JObject _) (JBoolean _) = GT
  compare (JObject _) (JNumber _) = GT
  compare (JObject _) (JString _) = GT
  compare (JObject _) (JArray _) = GT

-- | Case analysis for `Json` values. See the README for more information.
caseJson
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (String -> a)
  -> (String -> a)
  -> (Array Json -> a)
  -> (Object Json -> a)
  -> Json
  -> a
caseJson isUnit isBool isNum isStr isArr isObj = case _ of
  JNull -> isUnit unit
  JBoolean bool -> isBool bool
  JNumber num -> isNum num
  JString str -> isStr str
  JArray arr -> isArr arr
  JObject obj -> isObj obj

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was null, and a default value for all other cases.
caseJsonNull :: forall a. a -> (Unit -> a) -> Json -> a
caseJsonNull d f j = caseJson f (const d) (const d) (const d) (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Boolean`, and a default value for all other cases.
caseJsonBoolean :: forall a. a -> (Boolean -> a) -> Json -> a
caseJsonBoolean d f j = caseJson (const d) f (const d) (const d) (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Number`, and a default value for all other cases.
caseJsonNumber :: forall a. a -> (String -> a) -> Json -> a
caseJsonNumber d f j = caseJson (const d) (const d) f (const d) (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `String`, and a default value for all other cases.
caseJsonString :: forall a. a -> (String -> a) -> Json -> a
caseJsonString d f j = caseJson (const d) (const d) (const d) f (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Array Json`, and a default value for all other cases.
caseJsonArray :: forall a. a -> (Array Json -> a) -> Json -> a
caseJsonArray d f j = caseJson (const d) (const d) (const d) (const d) f (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was an `Object`, and a default value for all other cases.
caseJsonObject :: forall a. a -> (Object Json -> a) -> Json -> a
caseJsonObject d f j = caseJson (const d) (const d) (const d) (const d) (const d) f j

-- | Check if the provided `Json` is the `null` value
isNull :: Json -> Boolean
isNull = case _ of
  JNull -> true
  _ -> false

-- | Check if the provided `Json` is a `Boolean`
isBoolean :: Json -> Boolean
isBoolean = case _ of
  JBoolean _ -> true
  _ -> false

-- | Check if the provided `Json` is a `Number`
isNumber :: Json -> Boolean
isNumber = case _ of
  JNumber _ -> true
  _ -> false

-- | Check if the provided `Json` is a `String`
isString :: Json -> Boolean
isString = case _ of
  JString _ -> true
  _ -> false

-- | Check if the provided `Json` is an `Array`
isArray :: Json -> Boolean
isArray = case _ of
  JArray _ -> true
  _ -> false

-- | Check if the provided `Json` is an `Object`
isObject :: Json -> Boolean
isObject = case _ of
  JObject _ -> true
  _ -> false

-- | Convert `Json` to the `Unit` value if the `Json` is the null value
toNull :: Json -> Maybe Unit
toNull = case _ of
  JNull -> Just unit
  _ -> Nothing

-- | Convert `Json` to a `Boolean` value, if the `Json` is a boolean.
toBoolean :: Json -> Maybe Boolean
toBoolean = case _ of
  JBoolean bool -> Just bool
  _ -> Nothing

-- | Convert `Json` to a `Number` value, if the `Json` is a number.
toNumber :: Json -> Maybe Number
toNumber = case _ of
  JNumber num -> Number.fromString num
  _ -> Nothing

-- | Convert `Json` to a `String` value, if the `Json` is a string. To write a
-- | `Json` value to a JSON string, see `stringify`.
toString :: Json -> Maybe String
toString = case _ of
  JString str -> Just str
  _ -> Nothing

-- | Convert `Json` to an `Array` of `Json` values, if the `Json` is an array.
toArray :: Json -> Maybe (Array Json)
toArray = case _ of
  JArray arr -> Just arr
  _ -> Nothing

-- | Convert `Json` to an `Object` of `Json` values, if the `Json` is an object.
toObject :: Json -> Maybe (Object Json)
toObject = case _ of
  JObject obj -> Just obj
  _ -> Nothing

-- Encoding

-- | Construct `Json` from a `Boolean` value
fromBoolean :: Boolean -> Json
fromBoolean = JBoolean

-- | Construct `Json` from a `Number` value
fromNumber :: Number -> Json
fromNumber = JNumber <<< Number.toString

-- | Construct the `Json` representation of a `String` value.
-- | Note that this function only produces `Json` containing a single piece of `String`
-- | data (similar to `fromBoolean`, `fromNumber`, etc.).
-- | This function does NOT convert the `String` encoding of a JSON value to `Json` - For that
-- | purpose, you'll need to use `jsonParser`.
fromString :: String -> Json
fromString = JString

-- | Construct `Json` from an array of `Json` values
fromArray :: Array Json -> Json
fromArray = JArray

-- | Construct `Json` from an object with `Json` values
fromObject :: Object Json -> Json
fromObject = JObject

-- Defaults

-- | The JSON null value represented as `Json`
jsonNull :: Json
jsonNull = JNull

-- | The true boolean value represented as `Json`
jsonTrue :: Json
jsonTrue = fromBoolean true

-- | The false boolean value represented as `Json`
jsonFalse :: Json
jsonFalse = fromBoolean false

-- | The number zero represented as `Json`
jsonZero :: Json
jsonZero = fromNumber 0.0

-- | An empty string represented as `Json`
jsonEmptyString :: Json
jsonEmptyString = fromString ""

-- | An empty array represented as `Json`
jsonEmptyArray :: Json
jsonEmptyArray = fromArray []

-- | An empty object represented as `Json`
jsonEmptyObject :: Json
jsonEmptyObject = fromObject Obj.empty

-- | Constructs a `Json` array value containing only the provided value
jsonSingletonArray :: Json -> Json
jsonSingletonArray j = fromArray [ j ]

-- | Constructs a `Json` object value containing only the provided key and value
jsonSingletonObject :: String -> Json -> Json
jsonSingletonObject key val = fromObject (Obj.singleton key val)

-- | Converts a `Json` value to a JSON string. To retrieve a string from a `Json`
-- | string value, see `fromString`.
stringify :: Json -> String
stringify = case _ of
  JNull -> "null"
  JBoolean true -> "true"
  JBoolean false -> "false"
  JNumber num -> num
  JString str -> jsonString str
  JArray arr -> "[" <> (String.joinWith "," $ map stringify arr) <> "]"
  JObject obj -> "{" <> stringifyObject obj <> "}"
  where
    stringifyObject :: Object Json -> String
    stringifyObject obj = String.joinWith "," $ map (\(key /\ value) -> jsonString key <> ":" <> stringify value) $ Obj.toAscUnfoldable obj

foreign import jsonString :: String -> String

-- | Converts a `Json` value to a JSON string.
-- | The first `Int` argument specifies the amount of white space characters to use as indentation.
-- | This number is capped at 10 (if it is greater, the value is just 10). Values less than 1 indicate that no space should be used.
foreign import stringifyWithIndent :: Int -> Json -> String
