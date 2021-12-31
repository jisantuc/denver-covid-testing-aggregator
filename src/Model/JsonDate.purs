module Model.JsonDate
  ( JsonDate(..)
  , fromString
  , stringify
  , unsafeFromString
  , ymdFormat
  )
  where

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), encodeJson, toString)
import Data.Array (foldl, snoc, uncons)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, toCharArray)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Show, ($), (<$>), (<<<), (<>))

newtype JsonDate
  = JsonDate DateTime

derive newtype instance showJsonDate :: Show JsonDate

derive newtype instance eqJsonDate :: Eq JsonDate

dropTz :: String -> String
dropTz s = folder $ go [] (toCharArray s)
  where
  folder = foldl (\acc c -> acc <> singleton c) ""

  go acc chars = case uncons chars of
    Just { head: '+' } -> acc
    Just { head: 'Z' } -> acc
    Just { head, tail } -> go (acc `snoc` head) tail
    Nothing -> acc

adaptParseError :: String -> JsonDecodeError
adaptParseError s = TypeMismatch $ "String should match YYYY-MM-DDTHH:mm:SS format: " <> s

fromString :: String -> Either String JsonDate
fromString s =
  let
    withMillisTest = JsonDate <$> (unformat dateFormatWithoutMillis <<< dropTz) s

    withoutMillisTest = JsonDate <$> (unformat dateFormatWithoutMillis <<< dropTz) s
  in
    withMillisTest <|> withoutMillisTest

dateFormatWithoutMillis :: Formatter
dateFormatWithoutMillis =
  ( YearFull
      : (Placeholder "-")
      : MonthTwoDigits
      : (Placeholder "-")
      : DayOfMonthTwoDigits
      : (Placeholder "T")
      : Hours24
      : (Placeholder ":")
      : MinutesTwoDigits
      : (Placeholder ":")
      : SecondsTwoDigits
      : Nil
  )

ymdFormat :: Formatter
ymdFormat =  ( YearFull
      : (Placeholder "-")
      : MonthTwoDigits
      : (Placeholder "-")
      : DayOfMonthTwoDigits
      : Nil )

instance decodeJsonDate :: DecodeJson JsonDate where
  decodeJson js = case toString js of
    Just s -> lmap adaptParseError $ fromString s
    Nothing -> Left $ UnexpectedValue js

instance encodeJsonDate :: EncodeJson JsonDate where
  encodeJson (JsonDate dt) = encodeJson $ format ymdFormat dt

unsafeFromRight :: forall e a. Partial => Either e a -> a
unsafeFromRight (Right v) = v

unsafeFromString :: String -> JsonDate
unsafeFromString s = unsafePartial $ (unsafeFromRight <<< fromString) s

stringify :: JsonDate -> String
stringify (JsonDate dt) = format ymdFormat dt