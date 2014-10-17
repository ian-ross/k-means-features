{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude hiding (length, map, all, sum)
import qualified Prelude as P
import Control.Applicative
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Vector as G
import Data.Vector.Unboxed hiding ((++), forM_)
import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import CV.Image hiding (Complex)

import Data.KMeans.Config
import Data.KMeans.Feature
import Data.KMeans.Database

import qualified Data.Trees.KdTree as KD

instance KD.Point (String, Feature) where
  dimension (_, v) = length v
  coord i (_, v) = v ! i

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config] -> readConfig config >>= doit
    _ -> error "Usage: image-query <config-file>"

doit :: Config -> IO ()
doit cfg = do
  let imgdir = configImageDirectory cfg
  db <- openDatabase cfg
  putStrLn "Loading features from database..."
  imgfeatures <- loadImageFeatures db
  putStrLn $ "Images: " ++ show (P.length imgfeatures)
  putStrLn "Building name->feature map and feature kd-tree..."
  let names = M.fromList imgfeatures
      tree = KD.fromList imgfeatures
  putStrLn $ tree `seq` "Ready!"
  let loop near = do
        putStr $ show near ++ "> "
        hFlush stdout
        l <- getLine
        case words l of
          [] -> exitSuccess
          ["nearest", n] ->
            let newnear = read n :: Int
            in loop $ if newnear <= 0 then near else newnear
          [name] -> doQuery tree names near name
          _ -> putStrLn "Huh?"
        loop near
  loop 10

doQuery :: KD.KdTree (String, Feature) -> M.Map String Feature ->
           Int -> String -> IO ()
doQuery tr ns near name = do
  case M.lookup name ns of
    Nothing -> putStrLn "Image not found!"
    Just node -> do
      let neighs = KD.kNearestNeighbors tr near (name, node)
      forM_ neighs $ \(n, _) -> putStrLn n
      putStrLn $ "qiv " ++ (intercalate " " $
                            P.map (++ ".jpg") $ P.map fst neighs)
