{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified RSS as R
import qualified Text.Feed.Query as Q -- (getFeedTitle, getFeedItems, getItemTitle, getItemLink)
import Data.Maybe (fromJust)
import Text.Feed.Types (Item)

printItem :: Item -> IO ()
printItem item = do
  putStrLn "-----"
  print (fromJust $ Q.getItemTitle item)
  print (fromJust $ Q.getItemLink item)

main :: IO ()
main = do
  t <- R.getRssFeed "unixdigest.com" "feed.rss"
  let f = fromJust $ R.parseFeed t
      items = take 10 (Q.getFeedItems f)
  mapM_ printItem items
  pure ()
