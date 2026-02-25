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
import Types

configFile :: IO FilePath
configFile = do
  h <- getHomeDirectory
  return $ h ++ "/.config/hrsir_data.json"

getConfig :: FilePath -> IO (Either String Config)
getConfig path = do
  txt <- BL.readFile path
  return $ eitherDecode txt

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = do
  BL.writeFile path $ encode config
  return ()
