{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import RSS
import Brick
import Data.Text (unpack)
import Brick.Widgets.Border
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((%=), (.=), use)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

type FeedList = L.List String RssFeed
type ArticleList = L.List String Article

data UIBoxes = FeedsBox | ArticlesBox | ReadArticleBox
  deriving (Eq, Show)

data AppState = AppState
  { _feeds :: FeedList
  , _articles :: ArticleList
  , _selectedArticle :: Article
  , _focusedBox :: UIBoxes
  }

makeLenses ''AppState

makeFeedList :: RssFeedList -> L.List String RssFeed
makeFeedList fs = L.list "" (Vec.fromList fs) 1

drawUI :: AppState -> [Widget String]
drawUI st = let focused = st ^. focusedBox in
  case focused of
    ReadArticleBox -> [border $ drawArticle $ st ^. selectedArticle]
    _ -> [ hBox
           [ borderIf focused FeedsBox $ hLimit 60
             $ L.renderList drawFeedLinks (focused == FeedsBox) (st ^. feeds)
           , borderIf focused ArticlesBox $ hLimit 60
             $ L.renderList drawArticles (focused == ArticlesBox) (st ^. articles)
           ]
         ] where
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

handleEvents :: BrickEvent String e -> EventM String AppState ()
handleEvents (VtyEvent e) = case e of
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey x [] | x `elem` [V.KLeft, V.KRight]
                   -> focusedBox %= toggleFocus
      where
        toggleFocus FeedsBox = ArticlesBox
        toggleFocus ArticlesBox = FeedsBox
        toggleFocus ReadArticleBox = ArticlesBox
    V.EvKey V.KEnter [] -> do
      f <- use focusedBox
      case f of
        ArticlesBox -> openSelectedArticle
        _ -> return ()
      where
        openSelectedArticle = do
          sel <- use articles
          case  L.listSelectedElement sel of
            Nothing -> pure ()
            Just (_, art) -> do
              selectedArticle .= art
              focusedBox .= ReadArticleBox
    _ -> do
      fb <- use focusedBox
      case fb of
        FeedsBox -> zoom feeds $ L.handleListEvent e
        ArticlesBox -> zoom articles $ L.handleListEvent e
        _ -> pure ()
handleEvents _ = return ()

app :: App AppState e String
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvents
  , appStartEvent   = return ()
  , appAttrMap      = const $ attrMap V.defAttr [ (L.listSelectedAttr, V.black `on` V.white) ]
  }
