{-# LANGUAGE OverloadedStrings #-}
module Data.KMeans.Config ( Config(..), readConfig ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL


data Config = Config { configDbHost :: String
                     , configDbPort :: Int
                     , configDbDatabase :: String
                     , configDbUser :: String
                     , configDbPassword :: String
                     , configNPatches :: Int
                     , configNFeatures :: Int
                     , configFeatureSize :: Int
                     , configWhiten :: Bool
                     , configImageDirectory :: String
                     } deriving Show

instance FromJSON Config where
  parseJSON (Object v) = do
    db <- v .: "database"
    fs <- v .: "features"
    Config
      <$> db .: "host"
      <*> db .: "port"
      <*> db .: "name"
      <*> db .: "user"
      <*> db .: "password"
      <*> fs .: "patch_count"
      <*> fs .: "feature_count"
      <*> fs .: "feature_size" `mplus` return 6
      <*> fs .: "whiten_data" `mplus` return False
      <*> v  .: "image_directory"
  parseJSON _ = fail "Object expected"

readConfig :: FilePath -> IO Config
readConfig path = do
  configJson <- BSL.readFile path
  case eitherDecode configJson of
    Right config -> return config
    Left err -> error $ "Can't read the config file: " ++ err
