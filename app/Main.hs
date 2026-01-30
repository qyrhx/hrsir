{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import RSS (getRssFeed)

main :: IO ()
main = do
  resp <- getRssFeed "france24.com" "en/rss"
  putStrLn resp
