module UI where

import Types
import Event
import Brick
import Data.Text (unpack)
import Lens.Micro ((^.))
import Brick.Widgets.Border (border, hBorder)
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Brick.Widgets.List as L

errorAttr :: AttrName
errorAttr = attrName "error"

makeFeedList :: RssFeedList -> L.List String RssFeed
makeFeedList fs = L.list "" (Vec.fromList fs) 1

drawUI :: AppState -> [Widget String]
drawUI st = [vBox [drawMainUI st
                  , vLimit 1 $ drawCMD st]]

drawCMD :: AppState -> Widget String
drawCMD st = case st ^. cmd  of
  None -> fill ' '
  Message msg -> str $ unpack msg
  Err msg -> withAttr errorAttr $ str $ unpack msg
  Input t -> str $ ": " ++ unpack t

drawMainUI :: AppState -> Widget String
drawMainUI st = let focused = st ^. focusedBox in
  case focused of
    ReadArticleBox -> border $ drawArticle $ st ^. selectedArticle
    _ -> hBox
         [ borderIf focused FeedsBox
           $ L.renderList drawFeedLinks (focused == FeedsBox) (st ^. feeds)
         , borderIf focused ArticlesBox
           $ L.renderList drawArticles (focused == ArticlesBox) (st ^. articles)
         ]
      where
        borderIf f box w = if f == box then border w else padAll 1 w

drawArticle :: Article -> Widget String
drawArticle a = vBox [
  withAttr L.listSelectedAttr
    $ padRight Max $ str $ unpack $ articleTitle a
  , hBorder
  , str $ unpack $ articleContent a
  , fill ' '
  ]

drawArticles :: Bool -> Article -> Widget String
drawArticles sel a =
  let w = str (unpack $ articleTitle a)
  in if sel
       then withAttr L.listSelectedAttr w
       else w

drawFeedLinks :: Bool -> RssFeed -> Widget String
drawFeedLinks isSelected f =
  let w = str (unpack $ rssFeedUrl f)
  in if isSelected
       then withAttr L.listSelectedAttr w
       else w
app :: App AppState e String
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvents
  , appStartEvent   = return ()
  , appAttrMap      = const $ attrMap V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.white)
    , (errorAttr, fg V.red)
    , (errorAttr, fg V.red)
    ]
  }
