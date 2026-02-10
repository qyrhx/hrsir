module Main (main) where

import qualified Config as C
import Data.Maybe (fromJust)
import qualified RSS as R

main :: IO ()
--main = let initialState = UI.AppState { UI._feeds = (UI.makeFeedList $ replicate 5 dummyFeed) } in
--  void $ defaultMain UI.app initialState
main = do
  cf <- C.configFile
  c <- C.getConfig cf
  let conf = fromJust c
  fs <- R.getAllFeeds conf
  putStrLn $ show fs
  pure ()
