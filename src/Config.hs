module Config
  ( Config (..),
    configFile,
    getConfig,
    writeConfig,
  )
where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory
import Control.Exception (tryJust)
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)
import Types

configFile :: IO FilePath
configFile = do
  h <- getHomeDirectory
  return $ h ++ "/.config/hrsir_data.json"

getConfig :: FilePath -> IO (Either String Config)
getConfig path = do
  result <- tryJust (guard . isDoesNotExistError) (BL.readFile path)
  case result of
    Left ()   -> pure (Right defaultConfig)
    Right txt -> pure (eitherDecode txt)

writeConfig :: FilePath -> Config -> IO ()
writeConfig path cfg = do
  BL.writeFile path $ encode cfg
  return ()
