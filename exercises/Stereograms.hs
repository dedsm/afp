import Data.Char

type Color = Int
data RGB = RGB Color Color Color

type Image = [[RGB]]

validColor :: Color -> Bool
validColor color = color >=0 && color < 256

validRGB :: RGB -> Bool
validRGB (RGB red green blue) = validColor red && validColor green && validColor blue

validImage :: Image -> Maybe (Int, Int)
validImage [] = Just (0, 0)
validImage image@(x:xs) 
                | validRows image && validPixels image = Just (length x, length image)
                | otherwise = Nothing

validRows :: Image -> Bool
validRows [] = True
validRows (x:xs) = all ((==xlen).length) xs
                    where xlen = length x

validPixels :: Image -> Bool
validPixels [] = True
validPixels (x:xs) = all validRGB x && validPixels xs

ppmHeader :: (Int, Int) -> String
ppmHeader (w,h) = "P6 " ++ show w ++ " " ++ show h ++ " 255\n"

encodeRGB :: RGB -> String
encodeRGB (RGB red green blue) = map chr [red,green,blue]

ppmData :: Image -> String
ppmData image = map chr (concat image)
