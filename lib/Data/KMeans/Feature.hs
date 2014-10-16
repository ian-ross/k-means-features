module Data.KMeans.Feature
       ( Feature, Features, featureToImage, featureMontage
       , Patch, Patches, patchToImage, patchMontage
       , FeatureImage, FeatureImages, imageExtract
       ) where

import Prelude hiding (map, sum, zip3, concat, zipWith)
import qualified Prelude as P
import Control.Monad (forM_)
import Data.Vector.Unboxed hiding (forM_, (++))
import qualified Data.Vector as G
import CV.Image
import CV.Filters
import qualified CV.ImageMath as CV
import System.IO.Unsafe

import Data.KMeans.Config

type Grey = Image GrayScale D32
type Feature = Vector Double
type Features = G.Vector Feature
type FeatureImage = (Grey, Grey, Grey)
type FeatureImages = G.Vector FeatureImage
type Patch = Vector D8
type Patches = G.Vector Patch

featureToImage :: Int -> Feature -> FeatureImage
featureToImage sz feat =
  (getChannel Red img, getChannel Green img, getChannel Blue img)
  where img = featureToRGBImage sz feat

featureToRGBImage :: Int -> Feature -> Image RGB D32
featureToRGBImage sz feat = unsafePerformIO $ do
  let npx = sz * sz
      sl idx = map realToFrac $ slice (idx * npx) npx feat
      pxs = zip3 (sl 0) (sl 1) (sl 2)
  createWith (sz, sz) $ \img -> do
    forM_ [0..sz-1] $ \i ->
      forM_ [0..sz-1] $ \j -> setPixel (i, j) (pxs ! (i * sz + j)) img
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

featureMontage :: Int -> Features -> Image RGB D32
featureMontage sz feats = montage (rows, cols) 2 imgs
  where imgs = G.toList $ G.map (featureToRGBImage sz) feats
        n = G.length feats
        isqrt = truncate (sqrt $ fromIntegral n :: Double)
        cols = if isqrt * isqrt == n then isqrt else isqrt + 1
        rows = if n `mod` cols == 0 then n `div` cols else (n `div` cols) + 1

patchMontage :: Int -> Patches -> Image RGB D32
patchMontage sz feats = montage (rows, cols) 2 imgs
  where imgs = G.toList $ G.map (unsafeImageTo32F . patchToImage sz) feats
        n = G.length feats
        isqrt = truncate (sqrt $ fromIntegral n :: Double)
        cols = if isqrt * isqrt == n then isqrt else isqrt + 1
        rows = if n `mod` cols == 0 then n `div` cols else (n `div` cols) + 1


imageExtract :: Config -> FeatureImages -> Image RGB D32 -> Feature
imageExtract cfg feats img =
  concat $ G.toList $ G.map (featureExtract pooling chs) feats
  where pooling = configPoolingDivisor cfg
        chs = (getChannel Red img, getChannel Green img, getChannel Blue img)


featureExtract ::
  Int ->
  (Image GrayScale D32, Image GrayScale D32, Image GrayScale D32) ->
  FeatureImage -> Feature
featureExtract pooling (ir, ig, ib) (fr, fg, fb) =
  zipWith (+) (zipWith (+) pr pg) pb
  where cr = convolve2DI (-1, -1) ir fr
        cg = convolve2DI (-1, -1) ig fg
        cb = convolve2DI (-1, -1) ib fb
        pr = pool pooling cr
        pg = pool pooling cg
        pb = pool pooling cb

pool :: Int -> Image GrayScale D32 -> Feature
pool pooling img =
  let (w, h) = getSize img
      (poolw, poolh) = (w `div` pooling, h `div` pooling)
      sub i j = getRegion (i * poolw, j * poolh) (poolw, poolh)
  in fromList [ realToFrac $ CV.average $ sub i j img |
                i <- [0..pooling-1], j <- [0..pooling-1] ]


-- featConvolve :: Image GrayScale D32 -> Image GrayScale D32
--              -> Image GrayScale D32
-- featConvolve str f img =
--   let (fsz, _) = getSize f
--       (win, hin) = getSize img
--       (wout, hout) = ((win - fsz) `div` str + 1, (hin - fsz) `div` str + 1)
--   unsafePerformIO $ createWith (wout, hout) $ \oimg -> do
--     putStrLn "Convolving..."
--     forM_ [0..wout-1] $ \ix -> do
--       forM_ [0..hout-1] $ \iy -> do
--         let tmp = f `CV.mul` getRegion (ix * str, iy * str) (fsz, fsz) img
--             r = P.sum $
--                 P.map (CV.sum . flip getChannel tmp) [Red, Green, Blue]
--         setPixel (ix, iy) r oimg
--     return oimg
