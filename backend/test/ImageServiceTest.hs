module ImageServiceTest where

import Test.HUnit
import Services.ImageService
import Codec.Picture
import Servant.Multipart

data FileData = FileData String String String -- Si es necesario, mantener definiciones locales que no interfieran

-- Test case for rgbToGray
testRgbToGray :: Test
testRgbToGray = TestCase $ do
    assertEqual "RGB (255, 255, 255) to Gray" 255 (rgbToGray (PixelRGB8 255 255 255))
    assertEqual "RGB (0, 0, 0) to Gray" 0 (rgbToGray (PixelRGB8 0 0 0))
    assertEqual "RGB (76, 150, 29) to Gray" 114 (rgbToGray (PixelRGB8 76 150 29))

-- Test case for convertRGB8ToGray8
testConvertRGB8ToGray8 :: Test
testConvertRGB8ToGray8 = TestCase $ do
    let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 2 2
    let grayImg = convertRGB8ToGray8 img
    assertEqual "Pixel at (0,0) should be gray" 14 (pixelAt grayImg 0 0)
    assertEqual "Pixel at (1,1) should be gray" 15 (pixelAt grayImg 1 1)


-- Test case for rgbToGray with mid-range values
testRgbToGrayMidRange :: Test
testRgbToGrayMidRange = TestCase $ do
    assertEqual "RGB (127, 127, 127) to Gray" 127 (rgbToGray (PixelRGB8 127 127 127))
    assertEqual "RGB (80, 120, 160) to Gray" 112 (rgbToGray (PixelRGB8 80 120 160))

-- Test case for convertRGB8ToGray8 with a larger image
testConvertRGB8ToGray8Large :: Test
testConvertRGB8ToGray8Large = TestCase $ do
    let img = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 10)) (fromIntegral (y * 10)) 128) 10 10
    let grayImg = convertRGB8ToGray8 img
    assertEqual "Pixel at (5,5) should be gray" 59 (pixelAt grayImg 5 5)
    assertEqual "Pixel at (9,9) should be gray" 94 (pixelAt grayImg 9 9)

-- Test case for convertGrayImageToAscii with different gray levels
testConvertGrayImageToAsciiLevels :: Test
testConvertGrayImageToAsciiLevels = TestCase $ do
    let img = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 25)) (fromIntegral (x * 25)) (fromIntegral (x * 25))) 10 1
    let grayImg = convertRGB8ToGray8 img
    let asciiArt = convertGrayImageToAscii grayImg
    assertEqual "ASCII representation should vary with gray levels" "@@#8&o:*. <br>" asciiArt

-- Test case for convertGrayImageToAscii
testConvertGrayImageToAscii :: Test
testConvertGrayImageToAscii = TestCase $ do
    let img = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 25 + y * 25)) (fromIntegral (x * 25 + y * 25)) (fromIntegral (x * 25 + y * 25))) 10 1
    let grayImg = convertRGB8ToGray8 img
    let asciiArt = convertGrayImageToAscii grayImg
    assertEqual "ASCII representation should match expected" "@@#8&o:*. <br>" asciiArt

-- Test case for processImage
testProcessImage :: Test
testProcessImage = TestCase $ do
    let multipartData = undefined 
    let result = processImage multipartData
    assertBool "Result should be Left with error message" (isLeft result)

-- Helper function to check if a value is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Main function to run all tests
tests :: Test
tests = TestList
    [ testRgbToGray
    , testConvertRGB8ToGray8
    , testRgbToGrayMidRange
    , testConvertRGB8ToGray8Large
    , testConvertGrayImageToAsciiLevels
    , testConvertGrayImageToAscii
    , testProcessImage
    ]