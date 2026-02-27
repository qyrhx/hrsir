module UI where

import Brick
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.List (listSelectedElement)
import qualified Brick.Widgets.List as L
import Data.Maybe (fromJust)
import Data.Text (unpack)
import qualified Data.Vector as Vec
import Event
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Types

errorAttr :: AttrName
errorAttr = attrName "error"

msgAttr :: AttrName
msgAttr = attrName "msg"

makeFeedList :: RssFeedList -> L.List String RssFeed
makeFeedList fs = L.list "" (Vec.fromList fs) 1

drawUI :: AppState -> [Widget String]
drawUI st =
  [ vBox
      [ drawMainUI st,
        vLimit 1 $ drawCMD st
      ]
  ]

drawCMD :: AppState -> Widget String
drawCMD st = case st ^. cmd of
  None -> fill ' '
  Message msg -> withAttr msgAttr $ str $ " " ++ unpack msg
  Err msg -> withAttr errorAttr $ str $ " " ++ unpack msg
  Input t -> hBox [str $ ":" ++ unpack t, withAttr L.listSelectedAttr $ str " "]

drawMainUI :: AppState -> Widget String
drawMainUI st =
  let focused = st ^. focusedBox
   in case focused of
        ReadArticleBox -> border $ padAll 1 $ drawArticle $ st ^. selectedArticle
        _ ->
          hBox
            [ borderIfFocused FeedsBox $
                L.renderListWithIndex drawFeedLinks (focused == FeedsBox) (st ^. feeds),
              borderIfFocused ArticlesBox $
                L.renderList drawArticles (focused == ArticlesBox) $
                  (st ^. articles)
            ]
          where
            borderIfFocused box w = if focused == box then border w else padAll 1 w

articlesOfSelectedFeed :: AppState -> ArticleList
articlesOfSelectedFeed st =
  let fs = st ^. feeds
      (_, f) = fromJust $ listSelectedElement fs
      as = rssFeedArticles f
   in (L.list "X" (Vec.fromList as) 1)

drawArticle :: Article -> Widget String
drawArticle a =
  vBox
    [ withAttr L.listSelectedAttr $ padRight Max $ str $ unpack $ articleTitle a,
      hBorder,
      str $ unpack $ articleContent a,
      fill ' '
    ]

drawArticles :: Bool -> Article -> Widget String
drawArticles sel a =
  let w = str (unpack $ articleTitle a)
   in if sel then withAttr L.listSelectedAttr w else w

drawFeedLinks :: Int -> Bool -> RssFeed -> Widget String
drawFeedLinks idx isSelected f =
  let w = str $ show (1 + idx) ++ " - " ++ (unpack . rssFeedUrl $ f)
   in if isSelected
        then withAttr L.listSelectedAttr w
        else w

app :: App AppState e String
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvents,
      appStartEvent = return (),
      appAttrMap =
        const $
          attrMap
            V.defAttr
            [ (L.listSelectedAttr, V.black `on` V.white),
              (errorAttr, fg V.red),
              (msgAttr, fg V.yellow)
            ]
    }
