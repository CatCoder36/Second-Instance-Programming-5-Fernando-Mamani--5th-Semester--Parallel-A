module Services.ImageService(convertRGB8ToGray8, rgbToGray, pixelToChar, convertGrayImageToAscii,
processImage) where
import Servant.Multipart
import GHC.Generics (Generic)
import Codec.Picture
import Codec.Picture.Types (promoteImage)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B


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

-- Process an image to convert it to ASCII art
-- This function takes multipart data (such as that uploaded through a web form) and
-- processes the contained image file. First, it tries to extract exactly one file
-- from the multipart data. If there is not exactly one file, it returns an error,
-- then, it tries to decode the image file. If the decoding fails, it returns an
-- error, if the decoding succeeds, it converts the image to grayscale and then 
-- to ASCII art.
--
-- Parameters:
-- multipartData: The multipart data containing the image file
--
-- Returns: Either an error message or the ASCII representation of the image
processImage :: MultipartData Mem -> Either String String
processImage multipartData = do
    file <- maybeToEither "Expected exactly one file" $ singleFile multipartData
    scaleFactorString <- lookupInput (T.pack "scaleFactor") multipartData
    let scaleFactor = read (T.unpack scaleFactorString) :: Int
    dynamicImage <- decodeImageStrict (fdPayload file)
    let scaledImage = scaleImage dynamicImage scaleFactor
    let grayImage = convertRGB8ToGray8 (convertRGB8 scaledImage)
    return $ convertGrayImageToAscii grayImage

-- Extract a single file from multipart data
-- This function extracts a single file from the provided multipart data.
-- This function analyzes the multipart data and checks if it contains exactly
-- one file, if so, it returns `Just file`, where `file` is the only file found.
-- If the multipart data does not contain exactly one file (i.e. it contains
-- zero or more than one file), the function returns `Nothing`, indicating that 
-- a single file could not be extracted.
--
-- Parameters:
-- multipartData: The multipart data to analyze
--
-- Returns: `Just file` if the multipart data contains exactly one file, `Nothing` otherwise
singleFile :: MultipartData Mem -> Maybe (FileData Mem)
singleFile multipartData = case files multipartData of
    [file] -> Just file
    _      -> Nothing

-- Decode an image from a strict bytestring
-- This function attempts to decode an image from a strict `ByteString`.The
-- function takes a `ByteString` in lazy format (`L.ByteString`) as input and
-- converts it to strict format, then uses the `decodeImage` function to attempt
-- to decode this strict `ByteString` into a `DynamicImage`. If the decoding is 
-- successful, it returns `Right DynamicImage`, where `DynamicImage` is the type
-- of the decoded image. If it fails, it returns `Left String`, where the `String`
-- contains the error message.
--
-- Parameters:
-- bytestring: The bytestring to decode
--
-- Returns: Either the decoded image or an error message
decodeImageStrict :: L.ByteString -> Either String DynamicImage
decodeImageStrict = decodeImage . L.toStrict

-- Convert a `Maybe` value to an `Either` value
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just x) = Right x
maybeToEither e Nothing  = Left e

scaleImage :: DynamicImage -> Int -> DynamicImage
scaleImage (ImageRGB8 img) scaleFactor = ImageRGB8 $ scaleImageByFactor scaleFactor img
scaleImage (ImageRGBA8 img) scaleFactor = ImageRGBA8 $ scaleImageByFactor scaleFactor img
scaleImage (ImageYCbCr8 img) scaleFactor = ImageYCbCr8 $ scaleImageByFactor scaleFactor img

scaleImageByFactor :: Pixel a => Int -> Image a -> Image a
scaleImageByFactor factor img = generateImage generatePixel newX newY
  where
    newX = imageWidth img `div` factor
    newY = imageHeight img `div` factor
    generatePixel x y = pixelAt img (x * factor) (y * factor)