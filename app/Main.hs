module Main (main) where

import Brick (defaultMain)
import qualified Brick.Widgets.List as L
import Config
import Control.Monad (void)
import qualified Data.Vector as Vec
import RSS
import Types
import UI

main :: IO ()
main = do
  cf <- configFile
  c <- getConfig cf
  let conf = case c of
        (Right val) -> val
        (Left msg) -> error msg
  fs <- fetchAllFeeds conf
  let initState =
        AppState
          { _mode = Normal,
            _feeds = makeFeedList fs,
            _focusedBox = FeedsBox,
            _articles = undefined,
            _selectedArticle = head $ rssFeedArticles $ head fs,
            _cmd = None,
            _config = conf
          }
  let initState' = initState {_articles = articlesOfSelectedFeed initState}
  void $ defaultMain app initState'

main2 :: IO ()
main2 = do
  cf <- configFile
  c <- getConfig cf
  let conf = case c of
        (Right val) -> val
        (Left msg) -> error msg

  fs <- fetchAllFeeds conf
  -- let fs = replicate 8 dummyFeed

  let initState =
        AppState
          { _mode = Normal,
            _feeds = makeFeedList fs,
            _focusedBox = FeedsBox,
            _articles = (L.list "X" (Vec.fromList $ getAllArticles fs) 1),
            _selectedArticle = head $ rssFeedArticles $ head fs,
            _cmd = None,
            _config = conf
          }
  void $ defaultMain app initState

  pure ()
