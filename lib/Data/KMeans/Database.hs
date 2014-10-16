{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Data.KMeans.Database
       ( Connection, openDatabase
       , saveFeatures, loadFeatures
       , clearImageFeatures, saveImageFeatures
       ) where

import Prelude hiding (zip, length)
import Control.Monad
import Data.Vector (Vector)
import Data.Vector hiding (map, zipWith)
import qualified Data.Vector.Unboxed as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ

import Data.KMeans.Config
import Data.KMeans.Feature


openDatabase :: Config -> IO Connection
openDatabase cfg = do
  let conninfo =
        defaultConnectInfo { connectHost = configDbHost cfg
                           , connectPort = fromIntegral $ configDbPort cfg
                           , connectUser = configDbUser cfg
                           , connectPassword = configDbPassword cfg
                           , connectDatabase = configDbDatabase cfg }
  connect conninfo


saveFeatures :: Connection -> Features -> IO ()
saveFeatures conn fs = do
  let s :: [(Int, Vector Double)]
      s = zipWith (\i v -> (i, convert v)) [1..length fs] (toList fs)
  execute_ conn [sql|DELETE FROM features|]
  void $ executeMany conn [sql|INSERT INTO features (id, feature)
                                    VALUES (?, ?)|] s


loadFeatures :: Connection -> IO Features
loadFeatures conn = do
  fs <- query_ conn [sql|SELECT feature
                           FROM features
                          ORDER BY id|] :: IO [Only (Vector Double)]
  return $ fromList $ map (U.convert . fromOnly) fs


clearImageFeatures :: Connection -> IO ()
clearImageFeatures conn = void $ execute_ conn [sql|DELETE FROM images|]


saveImageFeatures :: Connection -> String -> Feature -> IO ()
saveImageFeatures conn name feat =
  void $ execute conn [sql|INSERT INTO images (id, features)
                                VALUES (?, ?)|]
                 (name, convert feat :: Vector Double)
