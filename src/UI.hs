{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import RSS
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Data.Text (Text, unpack, pack)

import Lens.Micro.TH (makeLenses)

-- TODO: use Lenses

type FeedList = L.List String RssFeed

data AppState = AppState
  { _feeds :: FeedList
  , _selected :: Int
  }

makeLenses ''AppState

makeFeedList :: RssFeedList -> L.List String RssFeed
makeFeedList feeds =
  L.list "" (Vec.fromList feeds) 1

drawFeed :: Bool -> RssFeed -> Widget String
drawFeed isSelected f =
  let w = str (unpack $ rssFeedUrl f)
  in if isSelected
       then withAttr L.listSelectedAttr w
       else w

drawUI :: AppState -> [Widget String]
drawUI st =
  [ border $
      hLimit 60 $
        L.renderList drawFeed True (_feeds st)
  ]

drawApp :: AppState -> [Widget String]
drawApp s = [hCenter $ hBox [L.renderList drawFeed True (_feeds s)]]

handleEvents :: BrickEvent String e -> EventM String AppState ()
handleEvents (VtyEvent e) = case e of
    V.EvKey (V.KChar 'q') [] -> halt
    _ -> do
        zoom feeds $ L.handleListEvent e
handleEvents _ = return ()

app :: App AppState e String
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvents
  , appStartEvent   = return ()
  , appAttrMap      = const $ attrMap V.defAttr [ (L.listSelectedAttr, V.black `on` V.white) ]
  }
