module Data.KMeans.Feature
       ( Feature, featureToImage, featureMontage
       , Patch, patchToImage, patchMontage
       ) where

import Prelude hiding (map, zip3)
import Control.Monad (forM_)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed hiding (forM_)
import qualified Data.Vector as G
import CV.Image
import System.IO.Unsafe

type Feature = Vector Double
type Patch = Vector D8

featureToImage :: Int -> Feature -> Image RGB D32
featureToImage sz feat = unsafePerformIO $ do
  let npx = sz * sz
      sl idx = map realToFrac $ slice (idx * npx) npx feat
      pxs = zip3 (sl 0) (sl 1) (sl 2)
  createWith (sz, sz) $ \img -> do
    forM_ [0..sz-1] $ \i ->
      forM_ [0..sz-1] $ \j ->
        setPixel (i, j) (pxs ! (i * sz + j)) img
    return img

patchToImage :: Int -> Patch -> Image RGB D8
patchToImage sz feat = unsafePerformIO $ do
  let npx = sz * sz
      sl idx = slice (idx * npx) npx feat
      pxs = zip3 (sl 0) (sl 1) (sl 2)
  createWith (sz, sz) $ \img -> do
    forM_ [0..sz-1] $ \i ->
      forM_ [0..sz-1] $ \j ->
        setPixel (i, j) (pxs ! (i * sz + j)) img
    return img

featureMontage :: Int -> G.Vector Feature -> Image RGB D32
featureMontage sz feats = montage (rows, cols) 2 imgs
  where imgs = G.toList $ G.map (featureToImage sz) feats
        n = G.length feats
        isqrt = truncate (sqrt $ fromIntegral n)
        cols = if isqrt * isqrt == n then isqrt else isqrt + 1
        rows = if n `mod` cols == 0 then n `div` cols else (n `div` cols) + 1

patchMontage :: Int -> G.Vector Patch -> Image RGB D32
patchMontage sz feats = montage (rows, cols) 2 imgs
  where imgs = G.toList $ G.map (unsafeImageTo32F . patchToImage sz) feats
        n = G.length feats
        isqrt = truncate (sqrt $ fromIntegral n)
        cols = if isqrt * isqrt == n then isqrt else isqrt + 1
        rows = if n `mod` cols == 0 then n `div` cols else (n `div` cols) + 1
