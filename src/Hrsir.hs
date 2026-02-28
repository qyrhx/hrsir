module Hrsir
  ( module Config,
    module Event,
    module RSS,
    module UI,
    module Types,
    startApp,
  )
where

import Config
import Event
import RSS
import Types
import UI

{- ORMOLU_DISABLE -}
-- Private
import Brick (defaultMain)
import Control.Monad (void)
{- ORMOLU_ENABLE -}

startApp :: IO ()
startApp = do
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
            _selectedArticle = undefined,
            _cmd = None,
            _config = conf
          }
  let initState' = initState {_articles = articlesOfSelectedFeed initState}
  void $ defaultMain app initState'
