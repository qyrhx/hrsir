module UI where

import Brick
import Brick.Widgets.Center (hCenter)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border.Style as BS

import RSS

-- TODO: use Lenses

data AppState = AppState
  { _feeds :: RssFeedList
  }

drawApp :: AppState -> [Widget n]
drawApp s =
  [ hCenter $ vBox
   [ str "Rss Feed Reader"
   , str $ replicate 20 '~'
   , withBorderStyle BS.unicodeBold $ vBox $ concatMap drawFeedTitles $ _feeds s
   ]
  ] where
  drawArticleTitle :: Article -> Widget n
  drawArticleTitle = str . title

  drawFeedTitles :: RssFeed -> [Widget n]
  drawFeedTitles f = map drawArticleTitle $ elems f


app :: App AppState e String
app = App
  { appDraw         = drawApp
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = resizeOrQuit
  , appStartEvent   = return ()
  , appAttrMap      = const $ attrMap V.defAttr []
  }
