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
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Process
import System.Process
import System.Random
import System.IO
import CV.Image hiding (Complex)
import Numeric.LinearAlgebra.HMatrix hiding (Vector, fromList, (!))

import Data.KMeans.Config
import Data.KMeans.Feature
import Data.KMeans.Database


main :: IO ()
main = do
  args <- getArgs
  case args of
    [config] -> readConfig config >>= doit
    _ -> error "Usage: build-features <config-file>"

doit :: Config -> IO ()
doit cfg = do
  let imgdir = configImageDirectory cfg
      npatches = configNPatches cfg
      nfeatures = configNFeatures cfg
      patchsize = configFeatureSize cfg
      whiten = configWhiten cfg
  images <- G.fromList <$> Prelude.map (imgdir </>) <$>
            filter (\p -> head p /= '.') <$> getDirectoryContents imgdir
  db <- openDatabase cfg
  putStrLn $ "Images: " ++ show (G.length images)
  putStrLn $ "npatches=" ++ show npatches ++ "  nfeatures=" ++ show nfeatures
  putStrLn $ "patchsize=" ++ show patchsize ++ "  whiten=" ++ show whiten
  putStrLn "Extracting patches..."
  patches <- G.replicateM npatches $ extractPatch patchsize images
  let normpatches = G.map (preprocess . map fromIntegral) patches
      gopatches = if whiten then whitenData normpatches else normpatches
  saveImage "orig-patch-montage.png" $ patchMontage patchsize patches
  saveImage "norm-patch-montage.png" $ featureMontage patchsize normpatches
  saveImage "preproc-patch-montage.png" $ featureMontage patchsize gopatches
  putStrLn "Clustering..."
  features <- doKMeans nfeatures gopatches
  putStrLn $ "#features=" ++ show (G.length features)
  saveImage "feature-montage.png" $ featureMontage patchsize features
  saveFeatures db features


-- Extract a random patch from a random image.
extractPatch :: Int -> G.Vector FilePath -> IO (Vector D8)
extractPatch sz imgs = do
  imgf <- (imgs G.!) <$> randomRIO (0, G.length imgs - 1)
  img <- readFromFile imgf :: IO (Image RGB D8)
  let (w, h) = getSize img
  px <- randomRIO (0, w - sz - 1)
  py <- randomRIO (0, h - sz - 1)
  let (r, g, b) = unzip3 $ getAllPixels $ getRegion (px, py) (sz, sz) img
  return $ fromList $ r ++ g ++ b


-- Per-patch local brightness and contrast normalisation.
preprocess :: Feature -> Feature
preprocess vin = map f vin
  where f = if all (== (vin ! 0)) vin then const 0 else \x -> (x - m) / sd
        n = fromIntegral $ length vin
        msq = sum (map (**2) vin) / n
        m = sum vin / n
        sd = sqrt $ msq - m * m


-- ZCA data whitening transform.
whitenData :: Features -> Features
whitenData xsin = fromMatrix $ w <> xs
  where xs = toMatrix xsin
        w = zca $ xs `mul` tr xs
        fromMatrix = (G.map S.convert) . G.fromList . toColumns
        toMatrix = fromColumns . G.toList . (G.map S.convert)
        zcaf x = if x < 0 then 0 else 1.0 / sqrt x
        zca m = let (l, v) = eigSH m
                in v `mul` diag (cmap zcaf l) `mul` inv v


-- Use R kmeans function for centroid calculation after (optional
-- whitening).
doKMeans :: Int -> Features -> IO Features
doKMeans nclust xs = do
  pid <- getProcessID
  let vtos x = intercalate " " $ G.toList $ G.map show $ G.convert x
      dfile = "tmp" ++ show pid ++ ".dat"
      cfile = "cen" ++ show pid ++ ".dat"
  withFile dfile WriteMode $ \h -> G.forM_ (G.map vtos xs) (hPutStrLn h)
  let rcmds = unlines
              [ "d <- read.table(\"" ++ dfile ++ "\")"
              , "res <- kmeans(d, " ++ show nclust ++
                ", iter.max=50, nstart=20)"
              , "write.table(res$centers, \"" ++ cfile ++ "\", " ++
                  "row.names=FALSE, col.names=FALSE)" ]
  (_, err) <- pshIn "R --slave" rcmds
  when (err /= "") $ error err
  dat <- readFile cfile
  removeFile dfile
  removeFile cfile
  let resconv s = G.convert $ G.map read $ G.fromList $ words s
  return $ G.map resconv $ G.fromList $ lines dat


-- Safely run a subsidiary process, passing input and collecting
-- output and error streams, set up so that: 1. we can compile a
-- multi-threaded version of it to prevent waitForProcess from
-- blocking everything, and 2. we can cause it to run the child
-- process in a separate process group so that we can easily kill all
-- descendant processes if our thread is killed.  Uses some lazy IO
-- tricks copied from System.Process.
--
pshIn :: String -> String -> IO (String, String)
pshIn cmd input =
  bracketOnError
  (createProcess $ (shell cmd) { std_in = CreatePipe
                               , std_out = CreatePipe
                               , std_err = CreatePipe
                               , create_group = True })
  (\(Just hin, Just hout, Just herr, ph) -> do
      interruptProcessGroupOf ph
      terminateProcess ph
      hClose hin
      _ <- slurp hout herr
      _ <- waitForProcess ph
      return $ ("Terminated", ""))
  (\(Just hin, Just hout, Just herr, ph) -> do
      hPutStrLn hin input
      hClose hin
      (sout, serr) <- slurp hout herr
      _ <- waitForProcess ph
      return (sout, serr))
  where slurp hout herr = do
          sout <- hGetContents hout ; serr <- hGetContents herr
          waitOut <- forkWait sout  ; waitErr <- forkWait serr
          waitOut                   ; waitErr
          hClose hout               ; hClose herr
          return (sout, serr)
        forkWait a = do
          res <- newEmptyMVar
          _ <- mask $ \restore ->
            forkIO $ try (restore $ evaluate $ rnf a) >>= putMVar res
          return (takeMVar res >>=
                  either (\ex -> throwIO (ex :: SomeException)) return)
