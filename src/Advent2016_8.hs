{-# LANGUAGE OverloadedStrings #-}

module Advent2016_8 where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad

type RowInt = Int
type ColumnInt = Int
type Position = (RowInt, ColumnInt)
    
data PixelState = On | Off deriving Eq

instance Show PixelState where
    show Off = "."
    show On = "#"

type Pixels = Map.Map Position PixelState

data Rotatable = Column | Row
data ScreenOperation 
    = Rectangle Int Int 
    | RotateColumn RowInt Int 
    | RotateRow ColumnInt Int
    deriving Show

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

screenTest = rect 2 2 . rotateRow 1 15 . rotateColumn 2 1 . rotateColumn 1 4 . rect 2 2 $ pixelMap
printScreenTest = printScreen screenTest

renderRows :: Pixels -> [String]
renderRows = map (mconcat . map show) . map Map.elems . pixelRows

renderScreen :: Pixels -> String
renderScreen = unlines . renderRows

printScreen :: Pixels -> IO ()
printScreen = putStrLn . renderScreen 


parseOperation :: T.Text -> ScreenOperation
parseOperation str
    | T.isPrefixOf "rect" str = Rectangle (read rectX :: Int) (read rectY :: Int)
    | T.isPrefixOf "rotate row" str = RotateRow (read rot :: ColumnInt) (read rotVal :: Int)
    | T.isPrefixOf "rotate column" str = RotateColumn (read rot :: RowInt) (read rotVal :: Int)
    where
        (op:s:vals) = T.words str
        (rot, rotVal) = (T.unpack . T.drop 2 . head $ vals, T.unpack . last $ vals)
        (rectX:rectY:[]) = map T.unpack $ T.splitOn "x" s


executeOperation :: ScreenOperation -> Pixels -> Pixels
executeOperation (Rectangle x y) = rect x y
executeOperation (RotateRow r n) = rotateRow r n
executeOperation (RotateColumn c n) = rotateColumn c n

operations :: [T.Text] -> [ScreenOperation]
operations inp = map parseOperation inp

input :: IO [T.Text]
input = do
    contents <- TIO.readFile "src\\input8_1.txt"
    return . T.lines $ contents

operationsIO :: IO [ScreenOperation]
operationsIO = operations <$> input

foldOps ::  [ScreenOperation] -> Pixels -> Pixels
foldOps [] p = p
foldOps (o:ops) p = foldOps ops (executeOperation o p)

foldOpsAnimate :: [ScreenOperation] -> Pixels -> IO (Pixels)
foldOpsAnimate [] p = printScreen p >> return p
foldOpsAnimate (o:ops) p = printScreen p
    >>  (foldOpsAnimate ops (executeOperation o p))

firstAnswer = Map.size <$> ((foldOps <$> operationsIO <*> return pixelMap) >>= (\pix -> return $ Map.filter (\y -> y == On) pix))
secondAnswer = (foldOps <$> operationsIO <*> return pixelMap) >>= (\pix -> printScreen pix)

animateOperations = foldOpsAnimate <$> operationsIO >>= (\y -> y pixelMap) >> return ()