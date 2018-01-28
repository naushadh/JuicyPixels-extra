{-# LANGUAGE RecordWildCards #-}

-- | 'PixelRGB16' version of 'scaleBilinear'
-- Lifted from juicy-pixel-processor
-- https://github.com/afcondon/juicy-pixel-processor/blob/master/app/Scaler16.hs
module Codec.Picture.Extra.Scaler16 (
  scaleBilinear
) where

import qualified Codec.Picture.Types as M
import           Codec.Picture.Types (PixelRGB16(..), Pixel16, Image(..))
import           Control.Monad.ST (runST)

-- | Scale an image using bi-linear interpolation. This is specialized to
-- 'PixelRGB16' only for speed.

scaleBilinear
  :: Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> Image PixelRGB16  -- ^ Original image
  -> Image PixelRGB16  -- ^ Scaled image
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
                    then PixelRGB16 0 0 0
                    else M.pixelAt img i j
            M.writePixel mimg x' y' $
              mulp16 (pixelAt' x y) ((1 - δx) * (1 - δy)) `addp16`
              mulp16 (pixelAt' (x + 1) y) (δx * (1 - δy)) `addp16`
              mulp16 (pixelAt' x (y + 1)) ((1 - δx) * δy) `addp16`
              mulp16 (pixelAt' (x + 1) (y + 1)) (δx * δy)
            go (x' + 1) y'
  go 0 0

mulp16 :: PixelRGB16 -> Float -> PixelRGB16
mulp16 pixel x = M.colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp16 #-}

addp16 :: PixelRGB16 -> PixelRGB16 -> PixelRGB16
addp16 = M.mixWith (const f)
  where
    f x y = fromIntegral $
      (0xff :: Pixel16) `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp16 #-}