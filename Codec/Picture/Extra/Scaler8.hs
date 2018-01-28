{-# LANGUAGE RecordWildCards #-}

module Codec.Picture.Extra.Scaler8 (
  scaleBilinear
) where

import qualified Codec.Picture.Types as M
import           Codec.Picture.Types (PixelRGB8(..), Pixel8, Image(..))
import           Control.Monad.ST (runST)

-- | Scale an image using bi-linear interpolation. This is specialized to
-- 'PixelRGB8' only for speed.

scaleBilinear
  :: Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> Image PixelRGB8   -- ^ Original image
  -> Image PixelRGB8   -- ^ Scaled image
scaleBilinear width height img@Image {..} = runST $ do
  mimg <- M.newMutableImage width height
  let sx, sy :: Float
      sx = fromIntegral imageWidth  / fromIntegral width
      sy = fromIntegral imageHeight / fromIntegral height
      go x' y'
        | x' >= width = go 0 (y' + 1)
        | y' >= height = M.unsafeFreezeImage mimg
        | otherwise = do
            let xf = fromIntegral x' * sx
                yf = fromIntegral y' * sy
                x, y :: Int
                x  = floor xf
                y  = floor yf
                δx = xf - fromIntegral x
                δy = yf - fromIntegral y
                pixelAt' i j =
                  if i >= imageWidth || j >= imageHeight
                    then PixelRGB8 0 0 0
                    else M.pixelAt img i j
            M.writePixel mimg x' y' $
              mulp (pixelAt' x y) ((1 - δx) * (1 - δy)) `addp`
              mulp (pixelAt' (x + 1) y) (δx * (1 - δy)) `addp`
              mulp (pixelAt' x (y + 1)) ((1 - δx) * δy) `addp`
              mulp (pixelAt' (x + 1) (y + 1)) (δx * δy)
            go (x' + 1) y'
  go 0 0

mulp :: PixelRGB8 -> Float -> PixelRGB8
mulp pixel x = M.colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}

addp :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
addp = M.mixWith (const f)
  where
    f x y = fromIntegral $
      (0xff :: Pixel8) `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp #-}