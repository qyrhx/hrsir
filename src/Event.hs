module Event where

import Brick
import qualified Brick.Widgets.List as L
import Control.Monad.IO.Class (liftIO)
import Data.Char (isPrint)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import System.Process
import Types

handleEvents :: BrickEvent String e -> EventM String AppState ()
handleEvents ev = do
  m <- use mode
  case m of
    Normal -> handleNormalModeEvent ev
    Command -> handleCommandModeEvent ev

-- Linux only, uses xdg
openInExternalBrowser :: String -> IO ()
openInExternalBrowser url = do
  _ <-
    createProcess
      (proc "xdg-open" [url])
        { std_out = NoStream,
          std_err = NoStream
        }
  pure ()

handleCommandModeEvent :: BrickEvent String e -> EventM String AppState ()
handleCommandModeEvent (VtyEvent (V.EvKey e [])) =
  let quitCmdMode = do
        mode .= Normal
        cmd .= None
   in case e of
        V.KEsc -> quitCmdMode
        V.KChar c | isPrint c -> cmd %= pushChar
          where
            pushChar (Input t) = Input $ T.snoc t c
            pushChar _ = error "WTF"
        V.KBS -> cmd %= delIfInput
          where
            delIfInput (Input t) = Input (T.dropEnd 1 t)
            delIfInput x = x
        V.KEnter -> do
          c <- use cmd
          case c of
            Input t -> do
              execCmd t
            _ -> pure ()
          where
            execCmd :: T.Text -> EventM String AppState ()
            execCmd c =
              let (w : ws) = T.words c
               in case w of
                    -- TODO: input validation
                    "add" -> do
                      modify (config . feedUrls %~ (++ [T.unpack $ head ws]))
                      cmd .= None
                      mode .= Normal
                    -- TODO
                    _ -> cmd .= Err "Unknown Command"
        _ -> pure ()
handleCommandModeEvent _ = pure ()

handleNormalModeEvent :: BrickEvent String e -> EventM String AppState ()
handleNormalModeEvent (VtyEvent kEv@(V.EvKey e [])) =
  case e of
    -- TODO: update config
    V.KChar 'q' -> halt
    V.KChar 'e' -> cmd .= Err "YO!!!!!! FILE DOES NOT EXIST"
    V.KChar 'm' -> cmd .= Message "File does exist!"
    V.KChar ':' -> do
      mode .= Command
      cmd .= Input ""
    x
      | x `elem` [V.KLeft, V.KRight] ->
          focusedBox %= toggleFocus
      where
        toggleFocus FeedsBox = ArticlesBox
        toggleFocus ArticlesBox = FeedsBox
        toggleFocus m = m
    V.KEnter -> do
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
          case L.listSelectedElement sel of
            Nothing -> pure ()
            Just (_, art) -> do
              selectedArticle .= art
              focusedBox .= ReadArticleBox
    _ -> do
      fb <- use focusedBox
      case fb of
        FeedsBox -> do
          zoom feeds $ L.handleListEvent kEv
          -- TODO: refactor this shit ffs
          fs <- use feeds
          let (_, f) = fromJust $ L.listSelectedElement fs
              as = rssFeedArticles f
          articles .= (L.list "X" (Vec.fromList as) 1)
        ArticlesBox -> zoom articles $ L.handleListEvent kEv
        _ -> pure ()
handleNormalModeEvent _ = pure ()
