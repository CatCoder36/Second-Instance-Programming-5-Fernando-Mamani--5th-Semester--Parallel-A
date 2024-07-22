module Services.ImageService(convertRGB8ToGray8, rgbToGray, pixelToChar, convertGrayImageToAscii) where

import GHC.Generics (Generic)
import Codec.Picture
import Codec.Picture.Types (promoteImage)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8

-- Convert a dynamic image to an RGB image
-- This function takes a color (RGB) image and 
-- converts it to a grayscale image. Use the `pixelMap` function to 
-- apply the `rgbToGray` function to each pixel in the original image,
-- thus transforming all RGB pixels to their corresponding grayscale 
-- values.
-- 
-- Parameters:
-- dynamicImage: The dynamic image to convert
--
-- Returns: The grayscale image
convertRGB8ToGray8 :: Image PixelRGB8 -> Image Pixel8
convertRGB8ToGray8 = pixelMap rgbToGray

-- Convert an RGB pixel to a grayscale pixel
-- This function converts an RGB pixel to a grayscale pixel.
-- It uses a formula that applies weights to the red, green, and blue
-- components of the RGB pixel, adding up to 30% red, 59% green, and 
-- 11% blue, to reflect the human eye's sensitivity to these colors.
-- The result is rounded to obtain the final grayscale value.
--
-- Parameters:
-- pixel: The RGB pixel to convert
--
-- Returns: The grayscale pixel
rgbToGray :: PixelRGB8 -> Pixel8
rgbToGray (PixelRGB8 r g b) = round (0.3 * fromIntegral r + 0.59 * fromIntegral g + 0.11 * fromIntegral b)


-- Convert a gray image to an ASCII string
-- This function converts a grayscale image to an ASCII string. 
-- Iterates over each row of the image, converting each pixel to
-- an ASCII character using `pixelToChar` and concatenating these
-- characters into a string. Each row is terminated with `<br>` to
-- represent a line break in HTML.
-- 
-- Parameters:
-- img: The grayscale image
--
-- Returns: The ASCII representation of the image as a string
convertGrayImageToAscii :: Image Pixel8 -> String
convertGrayImageToAscii img = concatMap rowToAscii [0 .. imageHeight img - 1]
    where
        rowToAscii y = map (\x -> pixelToChar (pixelAt img x y)) [0 .. imageWidth img - 1] ++ "<br>"

-- Defines the mapping of pixel values to ASCII characters
-- Parameters:
--  v: The pixel value
-- Returns: The ASCII character corresponding to the pixel value
pixelToChar :: Pixel8 -> Char
pixelToChar v
    | v < 26 = '@'
    | v < 51 = '#'
    | v < 76 = '8'
    | v < 101 = '&'
    | v < 126 = 'o'
    | v < 151 = ':'
    | v < 176 = '*'
    | v < 201 = '.'
    | v < 226 = ' '
    | otherwise = ' '