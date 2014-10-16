{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Data.KMeans.Database
       ( openDatabase, saveFeatures
       ) where

import Prelude hiding (zip, length)
import Control.Monad
import Data.Vector (Vector)
import Data.Vector hiding (zipWith)
import qualified Data.Vector.Unboxed as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ

import Data.KMeans.Config


openDatabase :: Config -> IO Connection
openDatabase cfg = do
  let conninfo =
        defaultConnectInfo { connectHost = configDbHost cfg
                           , connectPort = fromIntegral $ configDbPort cfg
                           , connectUser = configDbUser cfg
                           , connectPassword = configDbPassword cfg
                           , connectDatabase = configDbDatabase cfg }
  connect conninfo


saveFeatures :: Connection -> Vector (U.Vector Double) -> IO ()
saveFeatures conn fs = do
  let s :: [(Int, Vector Double)]
      s = zipWith (\i v -> (i, convert v)) [1..length fs] (toList fs)
  execute_ conn [sql|DELETE FROM features|]
  void $ executeMany conn
    [sql|INSERT INTO features (id, feature) VALUES (?, ?)|] s
