{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- This module provides two type classes: 'StringLike' and 'ToString' and
-- helper functions.
--
-- Type class 'StringLike' used for defining any string like
-- type that can be obtained via lazy 'LT.Text'. There are default
-- implementations for lazy 'LT.Text', strict 'ST.Text', lazy 'LB.ByteString'
-- and strict 'SB.ByteString'.
--
-- Type class 'ToString' used for defining a way to convert any type to
-- lazy 'LT.Text'.
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

-------------------------------------------------------------------------------
-- * Type classes

-- | This type class can be used to transform any string like from
-- lazy 'LT.Text', there is no default implementation for 'String' consciously,
-- beacause we don't want to incite 'String' using.
class StringLike a where
    fromLazyText :: LT.Text -> a

-- | This type class can be used to transform any type to 'StringLike' type.
-- Minimal complete definition: 'toText'.
class ToString a where
    toText :: a -> LT.Text

-------------------------------------------------------------------------------
-- * Utilities

-- | Transform any 'ToString' type to strict 'ST.Text'
text :: ToString a => a -> ST.Text
text = string

-- | Transform any 'ToString' type to lazy 'LT.Text'
ltext :: ToString a => a -> LT.Text
ltext = string

-- | Transform any 'ToString' type to strict 'SB.ByteString'
bs :: ToString a => a -> SB.ByteString
bs = string

-- | Transform any 'ToString' type to lazy 'LB.ByteString'
lbs :: ToString a => a -> LB.ByteString
lbs = string

-- | Transform any 'ToString' type to any 'StringLike' type, it can be inferred
-- or should be explicitly defined.
string  :: (ToString a, StringLike b) => a -> b
string = fromLazyText . toText

-------------------------------------------------------------------------------
-- * Instances

instance StringLike ST.Text where
    fromLazyText = LT.toStrict

instance StringLike LT.Text where
    fromLazyText = id

instance StringLike LB.ByteString where
    fromLazyText = LT.encodeUtf8

instance StringLike SB.ByteString where
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

instance ToString LT.Text where
    toText = id

instance ToString ST.Text where
    toText = LT.fromStrict

instance ToString SB.ByteString where
    toText = LT.fromStrict . ST.decodeUtf8

instance ToString LB.ByteString where
    toText = LT.decodeUtf8

-------------------------------------------------------------------------------
-- * Utils

lazyByteStringToStrict :: LB.ByteString -> SB.ByteString
#if MIN_VERSION_bytestring(0, 10, 0)
lazyByteStringToStrict = LB.toStrict
#else
lazyByteStringToStrict = SB.concat . LB.toChunks
#endif
{-# INLINE lazyByteStringToStrict #-}
