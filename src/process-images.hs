module Main where

import Prelude hiding (length, map, all, sum)
import qualified Prelude
import Control.Applicative
import Control.Monad
import qualified Data.Vector as G
import System.Directory
import System.Environment
import System.FilePath
import CV.Image hiding (Complex)

import Data.KMeans.Config
import Data.KMeans.Feature
import Data.KMeans.Database


main :: IO ()
main = do
  args <- getArgs
  case args of
    [config] -> readConfig config >>= doit
    _ -> error "Usage: process-images <config-file>"

doit :: Config -> IO ()
doit cfg = do
  images <- Prelude.map dropExtension <$>
            filter (\p -> head p /= '.') <$>
            (getDirectoryContents $ configImageDirectory cfg)
  db <- openDatabase cfg
  features <- G.map (featureToImage (configFeatureSize cfg)) <$> loadFeatures db
  putStrLn $ "Images: " ++ show (Prelude.length images)
  putStrLn $ "Features: " ++ show (G.length features)
  clearImageFeatures db
  forM_ images $ process cfg db features

process :: Config -> Connection -> FeatureImages -> String -> IO ()
process cfg db features imgname = do
  let imgf = configImageDirectory cfg </> imgname <.> "jpg"
  img <- readFromFile imgf
  let (w, h) = getSize img
  putStrLn $ "Processing " ++ imgf ++ " (" ++ show w ++ "x" ++ show h ++ ")"
  saveImageFeatures db imgname $ imageExtract cfg features img
