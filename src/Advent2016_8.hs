module Advent2016_8 where 

import Data.List
import qualified Data.Map as Map

type RowInt = Int
type ColumnInt = Int
type Position = (RowInt, ColumnInt)
    
data PixelState = On | Off

instance Show PixelState where
    show Off = "."
    show On = "#"

type Pixels = Map.Map Position PixelState

rowLength :: RowInt
rowLength = 50

columnHeight :: ColumnInt
columnHeight = 6

pixelMap :: Pixels
pixelMap = Map.fromList . map (\x -> (x, Off)) $ cartesianprod
    where
        cartesianprod = pure (,) <*> [0..rowLength-1] <*> [0..columnHeight-1]

pixelColumns' :: ColumnInt -> Pixels -> [Pixels]
pixelColumns' columnSize pix 
    | length pix == 0 = []
    | length pix < columnSize = [pix]
    | otherwise = row : pixelColumns' columnSize rest
    where
        (row, rest) = Map.splitAt (columnSize) pix

pixelColumnsToRows :: [Pixels] -> [Pixels]
pixelColumnsToRows columns = (map Map.fromList . transpose . map Map.toList) columns

pixelRows' :: ColumnInt -> Pixels -> [Pixels]
pixelRows' columnSize = pixelColumnsToRows . pixelColumns' columnSize

pixelRows :: Pixels -> [Pixels]
pixelRows = pixelRows' columnHeight

pixelColumns :: Pixels -> [Pixels]
pixelColumns = pixelColumns' rowLength --do not trust this

rect :: RowInt -> ColumnInt -> Pixels -> Pixels
rect 0 _ pix = pix
rect _ 0 pix = pix
rect a b pix = 
    let
        onCoords = pure (,) <*> [0..a-1] <*> [0..b-1]
        doUpdate [] screen = screen
        doUpdate (hed:xs) screen = doUpdate xs $ Map.adjust (\_ -> On) hed screen
    in
        doUpdate onCoords pix

rotateColumn :: RowInt -> Int -> Pixels -> Pixels
rotateColumn columnRowPos n pix =
    let
        newPos y = (y + n) `mod` columnHeight
        movePos (x, y) = (x, newPos y)
        columnVals = Map.mapKeys movePos $ getAllColumnCells columnRowPos pix
    in
        Map.union columnVals pix


getAllColumnCells :: RowInt -> Pixels -> Pixels
getAllColumnCells columnRowPos pix = Map.filterWithKey filterFunc pix
    where
        filterFunc (x, _) _ = x == columnRowPos
        


applyRects = rect 7 1 . rect 4 4 . rect 3 2 



renderRows :: Pixels -> [String]
renderRows = map (mconcat . map show) . map Map.elems . pixelRows

renderScreen :: Pixels -> String
renderScreen = unlines . renderRows

printScreen :: Pixels -> IO ()
printScreen = putStrLn . renderScreen 



