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

foldPixels :: [Pixels] -> Pixels
foldPixels = foldr Map.union (Map.singleton (0,0) Off)

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

data Rotatable = Column | Row

rotate :: Rotatable -> Int -> Int -> Pixels -> Pixels
rotate rtype columnRowPos n pix =
    let
        newPos Column y = (y + n) `mod` columnHeight
        newPos Row x = (x + n) `mod` rowLength
        movePos Column (x, y) = (x, newPos Column y)
        movePos Row (x, y) = (newPos Row x, y)
        columnVals = Map.mapKeys (movePos rtype) $ getAllRotatableCells rtype columnRowPos pix
    in
        Map.union columnVals pix

getAllRotatableCells :: Rotatable -> Int -> Pixels -> Pixels
getAllRotatableCells rtype pos pix = Map.filterWithKey (filterFunc rtype) pix
    where
        filterFunc Column (x, _) _ = x == pos
        filterFunc Row (_, y) _ = y == pos

rotateColumn :: RowInt -> Int -> Pixels -> Pixels
rotateColumn = rotate Column

rotateRow :: ColumnInt -> Int -> Pixels -> Pixels
rotateRow = rotate Row


screenTest = rotateRow 3 15 . rotateColumn 2 1 . rotateColumn 3 2 . rect 7 1 . rect 4 4 . rect 3 2 $ pixelMap
printScreenTest = printScreen screenTest


renderRows :: Pixels -> [String]
renderRows = map (mconcat . map show) . map Map.elems . pixelRows

renderScreen :: Pixels -> String
renderScreen = unlines . renderRows

printScreen :: Pixels -> IO ()
printScreen = putStrLn . renderScreen 



