{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- This module provides two type classes: 'StringLike' and 'ToString' and
-- helper functions.
--
-- Type class 'StringLike' used for defining any string like
-- type that can be obtained via lazy 'LazyText'. There are default
-- implementations for lazy 'LazyText', strict 'StrictText', lazy 'LazyByteString'
-- and strict 'StrictByteString'.
--
-- Type class 'ToString' used for defining a way to convert any type to
-- lazy 'LazyText'.
--
-- For example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main where
-- >
-- > import Data.ByteString.Lazy
-- >
-- > data Foo = Bar | Baz
-- >
-- > instance ToString Foo where
-- >     toText Bar = "bar"
-- >     toText Baz = "baz"
-- >
-- > test :: ByteString -> ()
-- > test = const ()
-- >
-- > main :: IO ()
-- > main = do
-- >     test $ string Bar
-- >     test $ lbs Baz
--

module Data.String.Like
    ( StringLike(..)
    , ToString(..)
    , string
    , text, ltext
    , bs, lbs
    ) where

import Prelude hiding (length)

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST

type StrictText = ST.Text
type LazyText = LT.Text
type StrictByteString = SB.ByteString
type LazyByteString = LB.ByteString

-------------------------------------------------------------------------------
-- * Type classes

-- | This type class can be used to transform any string like from
-- lazy 'LazyText', there is no default implementation for 'String' consciously,
-- beacause we don't want to incite 'String' using.
class StringLike a where
    fromLazyText :: LazyText -> a

-- | This type class can be used to transform any type to 'StringLike' type.
-- Minimal complete definition: 'toText'.
class ToString a where
    toText :: a -> LazyText

-------------------------------------------------------------------------------
-- * Utilities

-- | Transform any 'ToString' type to strict 'StrictText'
text :: ToString a => a -> StrictText
text = string

-- | Transform any 'ToString' type to lazy 'LazyText'
ltext :: ToString a => a -> LazyText
ltext = string

-- | Transform any 'ToString' type to strict 'StrictByteString'
bs :: ToString a => a -> StrictByteString
bs = string

-- | Transform any 'ToString' type to lazy 'LazyByteString'
lbs :: ToString a => a -> LazyByteString
lbs = string

-- | Transform any 'ToString' type to any 'StringLike' type, it can be inferred
-- or should be explicitly defined.
string  :: (ToString a, StringLike b) => a -> b
string = fromLazyText . toText

-------------------------------------------------------------------------------
-- * Instances

instance StringLike StrictText where
    fromLazyText = LT.toStrict

instance StringLike LazyText where
    fromLazyText = id

instance StringLike LazyByteString where
    fromLazyText = LT.encodeUtf8

instance StringLike StrictByteString where
    fromLazyText = lazyByteStringToStrict . fromLazyText

instance ToString Int where
    toText = toLazyText . decimal

instance ToString Int8 where
    toText = toLazyText . decimal

instance ToString Int16 where
    toText = toLazyText . decimal

instance ToString Int32 where
    toText = toLazyText . decimal

instance ToString Int64 where
    toText = toLazyText . decimal

instance ToString Word where
    toText = toLazyText . decimal

instance ToString Word8 where
    toText = toLazyText . decimal

instance ToString Word16 where
    toText = toLazyText . decimal

instance ToString Word32 where
    toText = toLazyText . decimal

instance ToString Word64 where
    toText = toLazyText . decimal

instance ToString Integer where
    toText = toLazyText . decimal

instance ToString Double where
    toText = toLazyText . realFloat

instance ToString Float where
    toText = toLazyText . realFloat

instance ToString String where
    toText = LT.pack

instance ToString LazyText where
    toText = id

instance ToString StrictText where
    toText = LT.fromStrict

instance ToString StrictByteString where
    toText = LT.fromStrict . ST.decodeUtf8

instance ToString LazyByteString where
    toText = LT.decodeUtf8

-------------------------------------------------------------------------------
-- * Internal Utils

lazyByteStringToStrict :: LazyByteString -> StrictByteString
#if MIN_VERSION_bytestring(0, 10, 0)
lazyByteStringToStrict = LB.toStrict
#else
lazyByteStringToStrict = SB.concat . LB.toChunks
#endif
{-# INLINE lazyByteStringToStrict #-}
