module Main where

import Test.Framework (defaultMain)

import qualified Data.String.Like.Tests

main :: IO ()
main = defaultMain
    [ Data.String.Like.Tests.tests
    ]
