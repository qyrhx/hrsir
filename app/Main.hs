module Main (main) where

import Types
import RSS
import UI
import Dummy
import Config
import Lens.Micro ((^.))
import Brick (defaultMain)
import Control.Monad (void)
import qualified Data.Vector as Vec
import qualified Brick.Widgets.List as L

main :: IO ()
main = do
  cf <- configFile
  c <- getConfig cf
  let conf = case c of
        (Right val) -> val
        (Left msg) -> error msg

  --fs <- getAllFeeds conf
  let fs = replicate 8 dummyFeed

  let initState = AppState {
        _feeds = makeFeedList fs
        , _focusedBox = FeedsBox
        , _articles = (L.list "X" (Vec.fromList $ rssFeedArticles $ head fs) 1)
        , _selectedArticle = head $ rssFeedArticles $ head fs
        }
  void $ defaultMain app initState

  pure ()
