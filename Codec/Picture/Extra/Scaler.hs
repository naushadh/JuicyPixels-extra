module Codec.Picture.Extra.Scaler where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPT
import qualified Codec.Picture.Extra.Scaler8 as Scaler8
import qualified Codec.Picture.Extra.Scaler16 as Scaler16
import           Control.Applicative ((<|>))

scaleBilinear :: Int -> Int -> JP.DynamicImage -> Either String JP.DynamicImage
scaleBilinear w h d
  = case go of
    (Just i) -> pure i
    Nothing  -> Left "Scaling not supported for: PixelF, PixelRGBF"
  where
    go8 :: JP.Image JP.PixelRGB8 -> JPT.DynamicImage
    go8 i = JP.ImageRGB8 $ Scaler8.scaleBilinear w h i
    go16 :: JP.Image JP.PixelRGB16 -> JPT.DynamicImage
    go16 i= JP.ImageRGB16 $ Scaler16.scaleBilinear w h i
    go = go8  <$> dynamicToRGB8  d
      <|> go16 <$> dynamicToRGB16 d

dropTransparency :: JPT.TransparentPixel a b => JP.Image a -> JP.Image b
dropTransparency = JPT.pixelMap JPT.dropTransparency

dynamicToRGB8 :: JP.DynamicImage -> Maybe (JP.Image JP.PixelRGB8)
dynamicToRGB8 (JP.ImageY8 i)    = pure . JPT.promoteImage $ i
dynamicToRGB8 (JP.ImageYA8 i)   = pure . JPT.promoteImage $ i
dynamicToRGB8 (JP.ImageRGB8 i)  = pure . id $ i
dynamicToRGB8 (JP.ImageRGBA8 i) = pure . dropTransparency $ i
dynamicToRGB8 (JP.ImageYCbCr8 i)= pure . JPT.convertImage $ i
dynamicToRGB8 (JP.ImageCMYK8 i) = pure . JPT.convertImage $ i
dynamicToRGB8 _ = Nothing

dynamicToRGB16 :: JP.DynamicImage -> Maybe (JP.Image JP.PixelRGB16)
dynamicToRGB16 (JP.ImageY16 i)    = pure . JPT.promoteImage $ i
dynamicToRGB16 (JP.ImageYA16 i)   = pure . JPT.promoteImage . dropTransparency $ i
dynamicToRGB16 (JP.ImageRGB16 i)  = pure . id $ i
dynamicToRGB16 (JP.ImageRGBA16 i) = pure . dropTransparency $ i
dynamicToRGB16 (JP.ImageCMYK16 i) = pure . JPT.convertImage $ i
dynamicToRGB16 _ = Nothing