{-# LANGUAGE CPP #-}

module Data.String.Like.Tests
       ( tests
       ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Instances ()
import qualified Data.Text as ST

import Data.String.Like

testText :: (Show a, ToString a) => a -> Bool
testText a = ST.pack (show a) == text a

testTextString :: String -> Bool
testTextString a = ST.pack a == text a

tests :: Test
tests = testGroup "Data.String.Like.Tests"
    [ testProperty "testText Integer" (testText :: Integer -> Bool)
    , testProperty "testText Int" (testText :: Int -> Bool)
#ifndef BADTEXT
    , testProperty "testText Int8" (testText :: Int8 -> Bool)
#endif
    , testProperty "testText Int16" (testText :: Int16 -> Bool)
    , testProperty "testText Int32" (testText :: Int32 -> Bool)
    , testProperty "testText Int64" (testText :: Int64 -> Bool)
    , testProperty "testText Word" (testText :: Word -> Bool)
    , testProperty "testText Word8" (testText :: Word8 -> Bool)
    , testProperty "testText Word16" (testText :: Word16 -> Bool)
    , testProperty "testText Word32" (testText :: Word32 -> Bool)
    , testProperty "testText Word64" (testText :: Word64 -> Bool)
    , testProperty "testText Double" (testText :: Double -> Bool)
    , testProperty "testText Float" (testText :: Float -> Bool)
    , testProperty "testText String" (testTextString :: String -> Bool)
    ]
