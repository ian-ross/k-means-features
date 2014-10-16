module Main where

import Prelude hiding (length, map, all, sum)
import qualified Prelude
import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Vector.Unboxed (fromList, length, map, all, sum, (!))
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector as G
import qualified Data.Vector.Storable as S
import Data.List (intercalate)
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Process
import System.Process
import System.Random
import System.IO
import CV.Image hiding (Complex)
import Numeric.LinearAlgebra.HMatrix hiding (Vector, fromList, (!))

import Debug.Trace

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
  features <- loadFeatures db
  putStrLn $ "Images: " ++ show (Prelude.length images)
  putStrLn $ "Features: " ++ show (G.length features)
  clearImageFeatures db
  forM_ images $ process cfg db features

process :: Config -> Connection -> Features -> String -> IO ()
process cfg db features imgname = do
  let imgf = configImageDirectory cfg </> imgname <.> "jpg"
  putStrLn $ "Processing " ++ imgf
  img <- readFromFile imgf
  saveImageFeatures db imgname $ imageExtract features img
