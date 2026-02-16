module Event where

import Types
import Brick
import System.Process
import Data.Char (isPrint)
import Lens.Micro.Mtl ((%=), (.=), use)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

handleEvents :: BrickEvent String e -> EventM String AppState ()
handleEvents ev = do
  m <- use mode
  case m of
    Normal -> handleNormalModeEvent ev
    Command -> handleCommandModeEvent ev

-- Linux only, uses xdg
openInExternalBrowser :: String -> IO ()
openInExternalBrowser url = do
  _ <- createProcess (proc "xdg-open" [url])
       { std_out = NoStream
       , std_err = NoStream
       }
  pure ()

handleCommandModeEvent :: BrickEvent String e -> EventM String AppState ()
handleCommandModeEvent (VtyEvent e) = case e of
   V.EvKey k [] | k `elem` [V.KEsc, V.KEnter] -> do
                    mode .= Normal
                    cmd .= None
   V.EvKey (V.KChar c) [] | isPrint c -> cmd %= pushChar
     where
       pushChar (Input t) = Input $ T.snoc t c
       pushChar _ = error "WTF"
   V.EvKey V.KBS [] -> cmd %= delIfInput
     where delIfInput (Input t) = Input (T.dropEnd 1 t)
           delIfInput x = x
   _ -> pure ()
handleCommandModeEvent _ = pure ()

handleNormalModeEvent :: BrickEvent String e -> EventM String AppState ()
handleNormalModeEvent (VtyEvent e) = case e of
   -- TODO: update config
   V.EvKey (V.KChar 'q') [] -> halt
   V.EvKey (V.KChar 'e') [] -> cmd .= Err "YO!!!!!! FILE DOES NOT EXIST"
   V.EvKey (V.KChar 'm') [] -> cmd .= Message "File does exist!"
   V.EvKey (V.KChar ':') [] -> do
     mode .= Command
     cmd .= Input ""
   V.EvKey x [] | x `elem` [V.KLeft, V.KRight]
                  -> focusedBox %= toggleFocus
     where
       toggleFocus FeedsBox = ArticlesBox
       toggleFocus ArticlesBox = FeedsBox
       toggleFocus m = m
   V.EvKey V.KEnter [] -> do
      f <- use focusedBox
      case f of
        ArticlesBox -> openSelectedArticle
        ReadArticleBox -> do
          a <- use selectedArticle
          _ <- liftIO $ openInExternalBrowser $ T.unpack $ articleUrl a
          pure ()
        _ -> pure ()
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

handleNormalModeEvent _ = pure ()
