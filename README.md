stringlike [![Build Status][travis-img]][travis]
==========

Transformations to several string-like types.

[travis]: http://travis-ci.org/selectel/stringlike
[travis-img]: https://secure.travis-ci.org/selectel/stringlike.png

Example
-------

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy

data Foo = Bar | Baz

instance ToString Foo where
    toText Bar = "bar"
    toText Baz = "baz"

test :: ByteString -> ()
test = const ()

main :: IO ()
main = do
    test $ string Bar
    test $ lbs Baz
```
