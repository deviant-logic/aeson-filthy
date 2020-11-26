{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Several newtypes and combinators for dealing with less-than-cleanly JSON input.

module Data.Aeson.Filthy
    (
     -- * Double-Encodings

      JSONString(..)
    , (.:$)
    , (.=$)

     -- * Booleans
     -- $booleans

    , OneOrZero(..)
    , YesOrNo(..)
    , OnOrOff(..)
    , AnyBool(..)

    -- * Maybe

    , EmptyAsNothing(..)

    -- * EmptyObject

    , EmptyObject(..)

    -- * Time
    , RFC2822Time(..)

    -- * Case Insensitive Keys

    , (.:~)

    ) where

import           Control.Applicative  (Alternative (..))
import           Control.Monad        (MonadPlus)
import           Control.Monad.Fix    (MonadFix)
import           Data.Aeson
import           Data.Aeson.Types     (Pair, Parser)
import           Data.Bits            (Bits, FiniteBits)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy    as HM
import           Data.Ix              (Ix)
import           Data.Monoid
import           Data.Semigroup       (Semigroup)
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Data.Time
import           Foreign.Storable     (Storable)
import           GHC.Generics         (Generic, Generic1)

-- | A double-encoded JSON value.
--
-- >>> encode (JSONString True)
-- "\"true\""
--
-- >>> decode "\"true\"" :: Maybe (JSONString Bool)
-- Just (JSONString {jsonString = True})
newtype JSONString a = JSONString { jsonString :: a }
    deriving (Bounded, Enum, Eq, Ord, Read, Show, Ix, Generic, FiniteBits, Bits, Storable, Num, Integral, Real, Floating, Fractional, RealFrac, RealFloat, IsString)

instance ToJSON a => ToJSON (JSONString a) where
    toJSON = String . T.decodeUtf8 . BL.toStrict . encode . jsonString

instance FromJSON a => FromJSON (JSONString a) where
    parseJSON = withText "a double-encoded json value (JSONString)"
                         (maybe (error "couldn't decode string") return . evil)
        where evil = fmap JSONString . decodeStrict . T.encodeUtf8

-- | Works like aeson's ('.:'), but assumes the value being parsed is double-encoded.  Mnemonic: @$@
--   sorta looks like an "S" (for "String").
(.:$) :: FromJSON a => Object -> Text -> Parser a
o .:$ t = jsonString <$> o .: t

-- | Works like aeson's ('.='), but double-encodes the value being serialized.
(.=$) :: ToJSON a => Text -> a -> Pair
n .=$ o = n .= JSONString o

-- $booleans There's a surprising number of ways people like to encode Booleans.  At present, the
--   docs below lie a bit in that values which don't parse to a 'True' value are considered false.
--   For instance,
--
-- >>> oneOrZero <$> decode "0"
-- Just False
--
-- >>> oneOrZero <$> decode "1"
-- Just True
--
-- >>> oneOrZero <$> decode "2"
-- Just False

-- | 'Bool's rendered "yes" or "no"
--
-- >>> yesOrNo <$> decode "\"yes\""
-- Just True
--
-- >>> yesOrNo <$> decode "\"no\""
-- Just False
newtype YesOrNo = YesOrNo { yesOrNo :: Bool }
    deriving (Bounded, Enum, Eq, Ord, Read, Show, Ix, Generic, FiniteBits, Bits, Storable)

-- | 'Bool's rendered "on" or "off"
--
-- >>> onOrOff <$> decode "\"on\""
-- Just True
--
-- >>> onOrOff <$> decode "\"off\""
-- Just False
newtype OnOrOff = OnOrOff { onOrOff :: Bool }
    deriving (Bounded, Enum, Eq, Ord, Read, Show, Ix, Generic, FiniteBits, Bits, Storable)

-- | 'Bool's rendered 0 or 1
--
-- >>> oneOrZero <$> decode "1"
-- Just True
--
-- >>> oneOrZero <$> decode "0"
-- Just False
newtype OneOrZero = OneOrZero { oneOrZero :: Bool }
    deriving (Bounded, Enum, Eq, Ord, Read, Show, Ix, Generic, FiniteBits, Bits, Storable)

-- | 'Bool's rendered as more-or-less anything.
--
-- >>> let Just bs = decode "[1, \"1\", \"true\", \"yes\", \"on\", true]"
-- >>> and $ map anyBool bs
-- True
newtype AnyBool = AnyBool { anyBool :: Bool }
    deriving (Bounded, Enum, Eq, Ord, Read, Show, Ix, Generic, FiniteBits, Bits, Storable)

instance ToJSON YesOrNo where
    toJSON (YesOrNo True) = "yes"
    toJSON _              = "no"

instance FromJSON YesOrNo where
    parseJSON "yes" = pure $ YesOrNo True
    parseJSON _     = pure $ YesOrNo False

instance ToJSON OnOrOff where
    toJSON (OnOrOff True) = "on"
    toJSON _              = "off"

instance FromJSON OnOrOff where
    parseJSON "on" = pure $ OnOrOff True
    parseJSON _    = pure $ OnOrOff False

instance ToJSON OneOrZero where
    toJSON (OneOrZero True) = Number 1
    toJSON _                = Number 0

instance FromJSON OneOrZero where
    parseJSON (Number 1)   = pure $ OneOrZero True
    parseJSON _            = pure $ OneOrZero False

instance FromJSON AnyBool where
    parseJSON (Number 1)      = pure $ AnyBool True
    parseJSON (String "1")    = pure $ AnyBool True
    parseJSON (String "true") = pure $ AnyBool True
    parseJSON (String "yes")  = pure $ AnyBool True
    parseJSON (String "on")   = pure $ AnyBool True
    parseJSON (Bool b)        = pure $ AnyBool b
    parseJSON _               = pure $ AnyBool False

-- | Sometimes an empty string in a JSON object actually means 'Nothing'
--
-- >>> emptyAsNothing <$> decode "\"\"" :: Maybe (Maybe Text)
-- Just Nothing
--
-- >>> emptyAsNothing <$> decode "\"something\"" :: Maybe (Maybe Text)
-- Just (Just "something")
newtype EmptyAsNothing a = EmptyAsNothing { emptyAsNothing :: Maybe a}
    deriving (Eq, Ord, Read, Show, Functor, Applicative, Alternative, Monad, MonadPlus, Foldable, Semigroup, Monoid, MonadFix, Generic, Generic1)

instance Traversable EmptyAsNothing where
    traverse f = fmap EmptyAsNothing . traverse f . emptyAsNothing

instance ToJSON a => ToJSON (EmptyAsNothing a) where
    toJSON = maybe "" toJSON . emptyAsNothing

instance FromJSON a => FromJSON (EmptyAsNothing a) where
    parseJSON "" = pure $ EmptyAsNothing Nothing
    parseJSON x  = EmptyAsNothing <$> parseJSON x

-- | Sometimes an empty object is all there is (/e.g./ returned instead of HTTP 204).
--
-- >>> decode "{}" :: Maybe EmptyObject
-- Just (EmptyObject {emptyObject = ()})
--
-- >> eitherDecode "{\"\":\"\"}" :: Either String EmptyObject
-- Left "Error in $: parsing EmptyObject failed, encountered non-empty Object"
--
-- >>> encode (EmptyObject ())
-- "{}"
newtype EmptyObject = EmptyObject { emptyObject :: () }
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance FromJSON EmptyObject where
  parseJSON = withObject "EmptyObject" $ \o ->
    if HM.null o
       then return $ EmptyObject ()
       else fail "parsing EmptyObject failed, encountered non-empty Object"

instance ToJSON EmptyObject where
  toJSON (EmptyObject ()) = object []

-- | A RFC 2822 encoded time value, allowing the modern RFC 2822 format.
-- These parsers do not currently handle the more messy whitespace and comments
-- allowed by the RFC.
--
-- The primary use for this if JSON APIs that dump JS datetimes as strings into the JSON.
--
-- Encoding follows the modern RFC 2822 recomended format:
--
-- >>> encode (RFC2822Time (read "2011-10-13 18:02:00 UTC"))
-- "\"Thu, 13 Oct 2011 18:02:00 +0000\""
--
-- Decoding though must be far more liberal:
--
-- >>> decode "\"Thu, 13 Oct 2011 18:02:00 GMT\"" :: Maybe RFC2822Time
-- Just (RFC2822Time {fromRFC2822Time = 2011-10-13 18:02:00 UTC})
-- >>> decode "\"Fri, 21 Nov 1997 09:55:06 -0600\"" :: Maybe RFC2822Time
-- Just (RFC2822Time {fromRFC2822Time = 1997-11-21 15:55:06 UTC})
-- >>> decode "\"Tue, 1 Jul 2003 10:52:37 +0200\"" :: Maybe RFC2822Time
-- Just (RFC2822Time {fromRFC2822Time = 2003-07-01 08:52:37 UTC})
-- >>> decode "\"Thu, 13 Feb 1969 23:32:54 -0330\"" :: Maybe RFC2822Time
-- Just (RFC2822Time {fromRFC2822Time = 1969-02-14 03:02:54 UTC})
--
-- RFC 822 obsolete dates:
--
-- >>> decode "\"21 Nov 97 09:55:06 GMT\"" :: Maybe RFC2822Time
-- Just (RFC2822Time {fromRFC2822Time = 0097-11-21 09:55:06 UTC})
--
-- Things that should parse but don't:
--
-- decode "\"Thu,\n      13\n        Feb\n          1969\n      23:32\n               -0330 (Newfoundland Time)\"" :: Maybe RFC2822Time
--
-- Just (RFC2822Time {fromRFC2822Time = 1969-02-14 03:02:00 UTC})
--
-- decode "\"Fri, 21 Nov 1997 09(comment):   55  :  06 -0600\"" :: Maybe RFC2822Time
--
-- Just (RFC2822Time {fromRFC2822Time = 1997-11-21 15:55:06 UTC})
newtype RFC2822Time
 = RFC2822Time { fromRFC2822Time :: UTCTime }
 deriving (Show, Eq, Ord, FormatTime)

instance FromJSON RFC2822Time where
  parseJSON = withText "RFC2822Time" $ \t ->
              -- Since RFC 2822 allows folding and optional whitespace, all whitespace is optional.
              -- Thus, to reduce the options, we parse the time stripped of whitespace.
              -- This allows some whitespace in places it isn't allowed. Also the RFC definition of
              -- whitespace is more permissive then spaces and newlines.
              let ts = T.unpack . T.replace " " "" . T.replace "\n" "" $ t
              in case getFirst . foldMap (\f -> parseTimeM True defaultTimeLocale f ts) $ (
                 do -- Generate the allows RFC2822 time format following its ABNF.
                   -- The day of week is optional
                   dow  <- ["", "%a,"]
                   date <- ["%e%b%Y", "%e%b%y"]
                   time <- ["%H:%M:%S", "%H:%M"]
                   zone <- ["%Z"]
                   pure (mconcat [dow, date, time, zone])) of
                        Just d -> pure (RFC2822Time d)
                        _      -> fail "could not parse RFC2822 format time"
  {-# INLINE parseJSON #-}

instance ToJSON RFC2822Time where
  toJSON = toJSON . rfc2822Time
  toEncoding = toEncoding . rfc2822Time

rfc2822Format :: String
rfc2822Format = "%a, %e %b %Y %T %z"

rfc2822Time :: RFC2822Time -> String
rfc2822Time = formatTime defaultTimeLocale rfc2822Format

-- | Some systems attempt to treat keys in JSON objects case-insensitively(ish).  Golang's JSON
--   marshalling is a prominent example: <https://golang.org/pkg/encoding/json/#Marshal>. The
--   ('.:~') combinator works like ('.:'), but if it fails to match, attempts to find a
--   case-insensitive variant of the key being sought.  If there is an exact match, ('.:~') will
--   take that; if there are multiple non-exact matches, the choice of selected value is
--   unspecified.  Mnemonic: @~@ swaps case in vi.
--
-- >>> data Foo = Foo Int deriving (Read, Show)
-- >>> instance FromJSON Foo where parseJSON (Object o) = Foo <$> o .:~ "foo"
-- >>> decode "{\"FOO\": 12}" :: Maybe Foo
-- Just (Foo 12)
-- >>> decode "{\"foo\": 17, \"FOO\": 12}" :: Maybe Foo
-- Just (Foo 17)
(.:~) :: FromJSON a => Object -> Text -> Parser a
o .:~ key = o .: key <|> maybe empty parseJSON go
    where go = lookup (T.toLower key) [(T.toLower k, v) | (k,v) <- HM.toList o]
