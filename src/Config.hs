{-# LANGUAGE DeriveGeneric #-}

module Config (Config(..), configFile, getConfig, writeConfig) where

import System.Directory
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL

data Config = Config {
  urls :: [String]
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

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
