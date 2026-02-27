module Event where

import Brick
import qualified Brick.Widgets.List as L
import Config
import Control.Monad.IO.Class (liftIO)
import Data.Char (isPrint)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import RSS
import System.Process
import Types

handleEvents :: BrickEvent String e -> EventM String AppState ()
handleEvents ev = do
  m <- use mode
  case m of
    Normal -> handleNormalModeEvent ev
    Command -> handleCommandModeEvent ev

handleCommandModeEvent :: BrickEvent String e -> EventM String AppState ()
handleCommandModeEvent (VtyEvent (V.EvKey e [])) =
  case e of
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
        Input t -> execCmd t
        _ -> pure ()
    _ -> pure ()
handleCommandModeEvent _ = pure ()

execCmd :: T.Text -> EventM String AppState ()
execCmd c =
  case validateCmdInput $ T.words c of
    Left errMsg -> do
      quitCmdMode
      cmd .= (Err errMsg)
    Right (command, arg) -> case command of
      "add" -> addFeedToConfig arg
      "del" -> deleteFeedFromConfig arg
      _ -> cmd .= Err "Unknown Command"

validateCmdInput :: [T.Text] -> Either T.Text (T.Text, T.Text)
validateCmdInput [] = Left "Nothing"
validateCmdInput (x : xs) = case x of
  c | c `elem` ["add", "del"] -> case xs of
    [] -> Left "Missing arguments"
    (y : _) -> Right (x, y)
  _ -> Left "Unknown Command"

handleNormalModeEvent :: BrickEvent String e -> EventM String AppState ()
handleNormalModeEvent (VtyEvent kEv@(V.EvKey e [])) =
  case e of
    V.KChar 'q' -> saveAndQuit
    V.KEsc -> do
      b <- use focusedBox
      case b of
        ReadArticleBox -> focusedBox .= FeedsBox
        _ -> pure ()
    V.KChar ':' -> do
      -- Switch to command mode
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
          liftIO $ openInExternalBrowser $ T.unpack $ articleUrl a
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
          updateSelectedArticles
        ArticlesBox -> zoom articles $ L.handleListEvent kEv
        _ -> pure ()
handleNormalModeEvent _ = pure ()

quitCmdMode :: EventM String AppState ()
quitCmdMode = do
  mode .= Normal
  cmd .= None

addFeedToConfig :: T.Text -> EventM String AppState ()
addFeedToConfig u = do
  modify (config . feedUrls %~ (++ [T.unpack u]))
  f <- liftIO $ fetchFeed u
  feeds %= \lst ->
    L.listInsert (Vec.length (L.listElements lst)) f lst
  quitCmdMode

deleteFeedFromConfig :: T.Text -> EventM String AppState ()
deleteFeedFromConfig u = do
  let idx = (read $ T.unpack u) - 1 :: Int
  fs <- use feeds
  if idx >= (Vec.length $ L.listElements fs)
    then do
      quitCmdMode
      cmd .= Err "Out of Range Index"
    else do
      feeds %= L.listRemove idx
      modify (config . feedUrls %~ deleteAtIdx idx)
      updateSelectedArticles
      quitCmdMode
  where
    deleteAtIdx i l = let (f, r) = splitAt i l in f ++ (safeTail r)

    safeTail [] = []
    safeTail (_ : xs) = xs

updateSelectedArticles :: EventM String AppState ()
updateSelectedArticles = do
  fs <- use feeds
  let (_, f) = fromJust $ L.listSelectedElement fs
      as = rssFeedArticles f
  articles .= L.list "X" (Vec.fromList as) 1

saveAndQuit :: EventM n AppState ()
saveAndQuit = do
  c <- use config
  cFile <- liftIO $ configFile
  liftIO $ writeConfig cFile c
  halt

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
