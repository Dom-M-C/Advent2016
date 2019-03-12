module Advent2016_8 where 

import Data.List
import qualified Data.Map as Map

type Position = (Int, Int)
    
data PixelState = On | Off

instance Show PixelState where
    show = renderPixel

type Pixels = Map.Map Position PixelState

switchState Off = On
switchState On = Off

turnOn :: PixelState -> PixelState
turnOn a = On

renderPixel :: PixelState -> String
renderPixel Off = "."
renderPixel On = "#"

renderRows :: Pixels -> [String]
renderRows = map (mconcat . map show) . map Map.elems . pixelRows

renderScreen :: Pixels -> String
renderScreen = unlines . renderRows

printScreen :: Pixels -> IO ()
printScreen = putStrLn . renderScreen 

rowLength :: Int
rowLength = 50

columnHeight :: Int
columnHeight = 6

cellCount = rowLength * columnHeight

pixelMap :: Pixels
pixelMap = Map.fromList . map (\x -> (x, Off)) $ cartesianprod
    where
        cartesianprod = pure (,) <*> [0..rowLength] <*> [0..columnHeight]
        restricted = take cellCount cartesianprod

pixelColumns' :: Int -> Pixels -> [Pixels]
pixelColumns' columnSize pix 
    | length pix == 0 = []
    | length pix < columnSize = [pix]
    | otherwise = row : pixelColumns' columnSize rest
    where
        (row, rest) = Map.splitAt (columnSize+1) pix

pixelColumnsToRows :: [Pixels] -> [Pixels]
pixelColumnsToRows columns = (map Map.fromList . transpose . map Map.toList) columns

pixelRows' :: Int -> Pixels -> [Pixels]
pixelRows' columnSize = pixelColumnsToRows . pixelColumns' columnSize

pixelRows :: Pixels -> [Pixels]
pixelRows = pixelRows' columnHeight

--pixelColumns :: Pixels -> [Pixels]
--pixelColumns = pixelColumns' rowLength --do not trust this

rect :: Pixels -> Int -> Int -> Pixels
rect pix 0 _ = pix
rect pix _ 0 = pix
rect pix a b = 
    let
        onCoords = pure (,) <*> [0..a-1] <*> [0..b-1]
        doUpdate [] screen = screen
        doUpdate (hed:xs) screen = doUpdate xs $ Map.adjust (\_ -> On) hed screen
    in
        doUpdate onCoords pix
        


{-


rect :: [Pixel] -> Int -> Int -> [Pixel]
rect p x y = undefined    
-}
--rotateRow :: Pixel -> Int -> 