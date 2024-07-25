module ImageServiceTest where

import Test.HUnit
import Services.ImageService
import Codec.Picture
import Servant.Multipart
import qualified Data.Text as T

data FileData = FileData String String String
-- Test case for rgbToGray
testRgbToGray :: Test
testRgbToGray = TestCase $ do
    assertEqual "RGB (255, 255, 255) to Gray" 255 (rgbToGray (PixelRGB8 255 255 255))
    assertEqual "RGB (0, 0, 0) to Gray" 0 (rgbToGray (PixelRGB8 0 0 0))
    assertEqual "RGB (76, 150, 29) to Gray" 114 (rgbToGray (PixelRGB8 76 150 29))
    assertEqual "RGB (128, 128, 128) to Gray" 128 (rgbToGray (PixelRGB8 128 128 128))
    assertEqual "RGB (255, 0, 0) to Gray" 76 (rgbToGray (PixelRGB8 255 0 0))
    assertEqual "RGB (0, 255, 0) to Gray" 150 (rgbToGray (PixelRGB8 0 255 0))
    assertEqual "RGB (0, 0, 255) to Gray" 28 (rgbToGray (PixelRGB8 0 0 255))
    assertEqual "RGB (100, 100, 100) to Gray" 100 (rgbToGray (PixelRGB8 100 100 100))
    assertEqual "RGB (200, 200, 200) to Gray" 200 (rgbToGray (PixelRGB8 200 200 200))
    assertEqual "RGB (50, 100, 150) to Gray" 90 (rgbToGray (PixelRGB8 50 100 150))
    assertEqual "RGB (20, 40, 60) to Gray" 36 (rgbToGray (PixelRGB8 20 40 60))
    assertEqual "RGB (240, 10, 10) to Gray" 79 (rgbToGray (PixelRGB8 240 10 10))
    assertEqual "RGB (10, 240, 10) to Gray" 146 (rgbToGray (PixelRGB8 10 240 10))
    assertEqual "RGB (10, 10, 240) to Gray" 35 (rgbToGray (PixelRGB8 10 10 240))

-- Test case for convertRGB8ToGray8
testConvertRGB8ToGray8 :: Test
testConvertRGB8ToGray8 = TestCase $ do
    let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 2 2
    let grayImg = convertRGB8ToGray8 img
    assertEqual "Pixel at (0,0) should be gray" 14 (pixelAt grayImg 0 0)
    assertEqual "Pixel at (1,1) should be gray" 15 (pixelAt grayImg 1 1)
    let img2 = generateImage (\x y -> PixelRGB8 100 100 100) 2 2
    let grayImg2 = convertRGB8ToGray8 img2
    assertEqual "Pixel at (0,0) with equal RGB should be gray" 100 (pixelAt grayImg2 0 0)
    assertEqual "Pixel at (1,1) with equal RGB should be gray" 100 (pixelAt grayImg2 1 1)
    let img3 = generateImage (\x y -> PixelRGB8 255 255 255) 2 2
    let grayImg3 = convertRGB8ToGray8 img3
    assertEqual "Pixel at (0,0) with max RGB should be white" 255 (pixelAt grayImg3 0 0)
    assertEqual "Pixel at (1,1) with max RGB should be white" 255 (pixelAt grayImg3 1 1)
    let img4 = generateImage (\x y -> PixelRGB8 0 0 0) 2 2
    let grayImg4 = convertRGB8ToGray8 img4
    assertEqual "Pixel at (0,0) with min RGB should be black" 0 (pixelAt grayImg4 0 0)
    assertEqual "Pixel at (1,1) with min RGB should be black" 0 (pixelAt grayImg4 1 1)


-- Test case for rgbToGray with mid-range values
testRgbToGrayMidRange :: Test
testRgbToGrayMidRange = TestCase $ do
    assertEqual "RGB (127, 127, 127) to Gray" 127 (rgbToGray (PixelRGB8 127 127 127))
    assertEqual "RGB (80, 120, 160) to Gray" 112 (rgbToGray (PixelRGB8 80 120 160))
    assertEqual "RGB (100, 100, 100) to Gray" 100 (rgbToGray (PixelRGB8 100 100 100))
    assertEqual "RGB (120, 140, 160) to Gray" 136 (rgbToGray (PixelRGB8 120 140 160))
    assertEqual "RGB (150, 130, 110) to Gray" 134 (rgbToGray (PixelRGB8 150 130 110))
    assertEqual "RGB (140, 140, 140) to Gray" 140 (rgbToGray (PixelRGB8 140 140 140))
    assertEqual "RGB (110, 150, 190) to Gray" 142 (rgbToGray (PixelRGB8 110 150 190))
    assertEqual "RGB (120, 110, 100) to Gray" 112 (rgbToGray (PixelRGB8 120 110 100))
    assertEqual "RGB (90, 100, 110) to Gray" 98 (rgbToGray (PixelRGB8 90 100 110))
    assertEqual "RGB (160, 140, 120) to Gray" 144 (rgbToGray (PixelRGB8 160 140 120))

-- Test case for convertRGB8ToGray8 with a larger image
testConvertRGB8ToGray8Large :: Test
testConvertRGB8ToGray8Large = TestCase $ do
    let img1 = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 10)) (fromIntegral (y * 10)) 128) 10 10
    let grayImg1 = convertRGB8ToGray8 img1
    assertEqual "Pixel at (0,0) should be gray" 14 (pixelAt grayImg1 0 0)
    assertEqual "Pixel at (5,5) should be gray" 59 (pixelAt grayImg1 5 5)
    assertEqual "Pixel at (9,9) should be gray" 94 (pixelAt grayImg1 9 9)
    assertEqual "Pixel at (2,3) should be gray" 38 (pixelAt grayImg1 2 3)
    assertEqual "Pixel at (7,8) should be gray" 82 (pixelAt grayImg1 7 8)
    let img2 = generateImage (\x y -> PixelRGB8 128 128 (fromIntegral (x * 10))) 10 10
    let grayImg2 = convertRGB8ToGray8 img2
    assertEqual "Pixel at (0,0) should be gray" 114 (pixelAt grayImg2 0 0)
    assertEqual "Pixel at (5,5) should be gray" 119 (pixelAt grayImg2 5 5)
    assertEqual "Pixel at (9,9) should be gray" 124 (pixelAt grayImg2 9 9)
    assertEqual "Pixel at (2,3) should be gray" 116 (pixelAt grayImg2 2 3)
    assertEqual "Pixel at (7,8) should be gray" 122 (pixelAt grayImg2 7 8)
    let img3 = generateImage (\x y -> if (x + y) `mod` 2 == 0 then PixelRGB8 0 0 0 else PixelRGB8 255 255 255) 10 10
    let grayImg3 = convertRGB8ToGray8 img3
    assertEqual "Pixel at (0,0) should be black" 0 (pixelAt grayImg3 0 0)
    assertEqual "Pixel at (1,1) should be white" 0 (pixelAt grayImg3 1 1)
    assertEqual "Pixel at (2,2) should be black" 0 (pixelAt grayImg3 2 2)
    assertEqual "Pixel at (3,3) should be white" 0 (pixelAt grayImg3 3 3)
    assertEqual "Pixel at (4,4) should be black" 0 (pixelAt grayImg3 4 4)
    

-- Test case for convertGrayImageToAscii with different gray levels
testConvertGrayImageToAsciiLevels :: Test
testConvertGrayImageToAsciiLevels = TestCase $ do
    let img1 = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 25)) (fromIntegral (x * 25)) (fromIntegral (x * 25))) 10 1
    let grayImg1 = convertRGB8ToGray8 img1
    let asciiArt1 = convertGrayImageToAscii grayImg1
    assertEqual "ASCII representation should vary with gray levels" "@@#8&o:*. <br>" asciiArt1
    let img2 = generateImage (\x y -> PixelRGB8 128 128 128) 10 1
    let grayImg2 = convertRGB8ToGray8 img2
    let asciiArt2 = convertGrayImageToAscii grayImg2
    assertEqual "ASCII representation should be all middle gray" "::::::::::<br>" asciiArt2
    let img3 = generateImage (\x y -> PixelRGB8 (fromIntegral x * 25) (fromIntegral x * 25) (fromIntegral x * 25)) 10 1
    let grayImg3 = convertRGB8ToGray8 img3
    let asciiArt3 = convertGrayImageToAscii grayImg3
    assertEqual "ASCII representation should vary from black to white" "@@#8&o:*. <br>" asciiArt3
    let img4 = generateImage (\x y -> if x `mod` 2 == 0 then PixelRGB8 0 0 0 else PixelRGB8 255 255 255) 10 1
    let grayImg4 = convertRGB8ToGray8 img4
    let asciiArt4 = convertGrayImageToAscii grayImg4
    assertEqual "ASCII representation should alternate between black and white" "@ @ @ @ @ <br>" asciiArt4
    let img5 = generateImage (\x y -> PixelRGB8 (fromIntegral (y * 25)) (fromIntegral (y * 25)) (fromIntegral (y * 25))) 1 10
    let grayImg5 = convertRGB8ToGray8 img5
    let asciiArt5 = convertGrayImageToAscii grayImg5
    assertEqual "ASCII representation should be vertical gradient" "@<br>@<br>#<br>8<br>&<br>o<br>:<br>*<br>.<br> <br>" asciiArt5


-- Test case for convertGrayImageToAscii
testConvertGrayImageToAscii :: Test
testConvertGrayImageToAscii = TestCase $ do
    let img1 = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 25 + y * 25)) (fromIntegral (x * 25 + y * 25)) (fromIntegral (x * 25 + y * 25))) 10 1
    let grayImg1 = convertRGB8ToGray8 img1
    let asciiArt1 = convertGrayImageToAscii grayImg1
    assertEqual "ASCII representation should match expected for linear gradient" "@@#8&o:*. <br>" asciiArt1
    let img2 = generateImage (\x y -> PixelRGB8 50 50 50) 10 1
    let grayImg2 = convertRGB8ToGray8 img2
    let asciiArt2 = convertGrayImageToAscii grayImg2
    assertEqual "ASCII representation should be uniform gray" "##########<br>" asciiArt2
    let img3 = generateImage (\x y -> PixelRGB8 (fromIntegral (225 - x * 25)) (fromIntegral (225 - x * 25)) (fromIntegral (225 - x * 25))) 10 1
    let grayImg3 = convertRGB8ToGray8 img3
    let asciiArt3 = convertGrayImageToAscii grayImg3
    assertEqual "ASCII representation should be reverse gradient" " .*:o&8#@@<br>" asciiArt3
    let img4 = generateImage (\x y -> if x `mod` 2 == 0 then PixelRGB8 0 0 0 else PixelRGB8 255 255 255) 10 1
    let grayImg4 = convertRGB8ToGray8 img4
    let asciiArt4 = convertGrayImageToAscii grayImg4
    assertEqual "ASCII representation should alternate between black and white" "@ @ @ @ @ <br>" asciiArt4
    let img5 = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 25)) (fromIntegral (x * 25)) (fromIntegral (x * 25))) 10 10
    let grayImg5 = convertRGB8ToGray8 img5
    let asciiArt5 = convertGrayImageToAscii grayImg5
    assertEqual "ASCII representation should be vertical gradient" "@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>@@#8&o:*. <br>" asciiArt5


-- Main function to run all tests
tests :: Test
tests = TestList
    [ testRgbToGray
    , testConvertRGB8ToGray8
    , testRgbToGrayMidRange
    , testConvertRGB8ToGray8Large
    , testConvertGrayImageToAsciiLevels
    , testConvertGrayImageToAscii
    ]