module UI where

import Brick
import qualified Brick.Widgets.Border.Style as BS

import RSS

-- TODO: use Lenses

data AppState = AppState
  { _feeds :: RssFeedList
  }

drawApp :: AppState -> [Widget n]
drawApp s =
  [hBox
   [withBorderStyle BS.unicodeBold $ vBox $ concatMap drawFeedTitles $ _feeds s]
  ] where
  drawArticleTitle :: Article -> Widget n
  drawArticleTitle = str . title

  drawFeedTitles :: RssFeed -> [Widget n]
  drawFeedTitles f = map drawArticleTitle $ elems f
