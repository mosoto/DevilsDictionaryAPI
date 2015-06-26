{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Safe


main :: IO ()
main = do
    runSpock 5000 $ spockT id $ do
        get root $ html "<h1>Hello World</h1>"
