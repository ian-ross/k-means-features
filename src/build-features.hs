module Main where

import Control.Applicative
import Control.Monad
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Directory
import System.FilePath
import System.Console.CmdTheLine
import System.Random
import CV.Image

import Data.KMeans.Feature


main :: IO ()
main = run (doit <$> imgdir
                 <*> npatches
                 <*> nfeatures
                 <*> patchsize
                 <*> whiten,
            defTI)
  where imgdir = value . opt "." $ optInfo ["image-directory", "d"]
        npatches = value . opt 1000 $ optInfo ["patch-count", "p"]
        nfeatures = value . opt 100 $ optInfo ["feature-count", "f"]
        patchsize = value . opt 6 $ optInfo ["patch-size", "s"]
        whiten = value . flag $ optInfo ["whiten", "w"]

doit :: FilePath -> Int -> Int -> Int -> Bool -> IO ()
doit imgdir npatches nfeatures patchsize whiten = do
  images <- V.fromList <$> map (imgdir </>) <$>
            filter (\p -> head p /= '.') <$> getDirectoryContents imgdir
  putStrLn $ "Images: " ++ show (V.length images)
  putStrLn $ "npatches=" ++ show npatches ++ "  nfeatures=" ++ show nfeatures
  putStrLn $ "patchsize=" ++ show patchsize ++ "  whiten=" ++ show whiten
  patches <- replicateM npatches $ extractPatch patchsize images
  let normpatches = map preprocess patches
  forM_ normpatches $ \i ->
    putStrLn $ show (V.sum i / fromIntegral (V.length i))

-- Extract a random patch from a random image.
extractPatch :: Int -> Vector FilePath -> IO (Vector D8)
extractPatch sz imgs = do
  imgf <- (imgs V.!) <$> randomRIO (0, V.length imgs - 1)
  img <- readFromFile imgf :: IO (Image RGB D8)
  let (w, h) = getSize img
  px <- randomRIO (0, w - sz - 1)
  py <- randomRIO (0, h - sz - 1)
  let (r, g, b) = unzip3 $ getAllPixels $ getRegion (px, py) (sz, sz) img
  return $ V.fromList $ r ++ g ++ b

-- Brightness and contrast normalisation.
preprocess :: Vector D8 -> Vector Double
preprocess vin = V.map (\x -> (x - m) / sd) dvin
  where n = fromIntegral (V.length vin)
        dvin = V.map fromIntegral vin :: Vector Double
        tot = V.sum dvin
        totsq = V.sum $ V.map (**2) dvin
        m = tot / n
        msq = totsq / n
        sd = sqrt $ msq - m * m
