{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified UI as UI
import qualified RSS as R
import qualified Text.Feed.Query as Q -- (getFeedTitle, getFeedItems, getItemTitle, getItemLink)
import Data.Maybe (fromJust)
import Text.Feed.Types (Item)
import Brick.Main (defaultMain)
import Control.Monad (void)

printItem :: Item -> IO ()
printItem item = do
  putStrLn "-----"
  print (fromJust $ Q.getItemTitle item)
  print (fromJust $ Q.getItemLink item)

dummyArticles :: [R.Article]
dummyArticles =
  [ R.Article
      { R.title = "Haskell 9.10 Released"
      , R.content = "Lots of performance improvements and new warnings."
      , R.read = False
      }
  , R.Article
      { R.title = "Rust Dev Discovers Monads"
      , R.content = "Claims it is 'just like Option but scary'."
      , R.read = False
      }
  , R.Article
      { R.title = "Programmer Refactors For Fun, Regrets Everything"
      , R.content = "Spent 6 hours, gained 0 features, but code is now 'clean'."
      , R.read = True
      }
  ]

dummyFeed :: R.RssFeed
dummyFeed =
  R.RssFeed
    { R.url = "https://example.com/feed.xml"
    , R.elems = dummyArticles
    }


main :: IO ()
main = let initialState = UI.AppState { UI._feeds = (replicate 5 dummyFeed) } in
  void $ defaultMain UI.app initialState
--main = do
--  t <- getRssFeed "unixdigest.com" "feed.rss"
--  let f = fromJust $ parseFeed t
--      items = take 10 (Q.getFeedItems f)
--  mapM_ printItem items
--  pure ()
