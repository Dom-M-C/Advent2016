module Advent2016_6 where 

import qualified Data.Map as Map
import Data.List

type ColumnIndex = Int
type ColumnString = String

data ColumnStrings = ColumnStrings (Map.Map ColumnIndex ColumnString)

buildColumnString :: Char -> Maybe ColumnString -> ColumnString
buildColumnString c Nothing = [c]
buildColumnString c (Just cs) = c:cs

buildRowMap :: String -> ColumnIndex -> Map.Map ColumnIndex ColumnString -> Map.Map ColumnIndex ColumnString 
buildRowMap [] _ m = m
buildRowMap (c:cs) idx m = 
    let
        columnString = buildColumnString c $ Map.lookup idx m
        updatedMap = (Map.insert idx columnString m)
        nextIndex = idx + 1
    in
        buildRowMap cs nextIndex updatedMap
        
buildColumnMap :: [String] -> Map.Map ColumnIndex ColumnString -> ColumnStrings 
buildColumnMap [] m = ColumnStrings m
buildColumnMap (row:rows) m = buildColumnMap rows (buildRowMap row 0 m)

showColumnStrings :: ColumnStrings -> String
showColumnStrings (ColumnStrings cmap)
    | cmap == Map.empty = "<empty>"
    | otherwise = mconcat . map columnPairToString $ Map.toList cmap

columnPairToString :: (ColumnIndex, ColumnString) -> String
columnPairToString (x, y) = "Position " <> (show x) <> ": " <> y <> "\n"

instance Show ColumnStrings where
    show = showColumnStrings         
    
    

input = 
    [   "eedadn"
    ,   "drvtee"
    ,   "eandsr"
    ,   "raavrd"
    ,   "atevrs"
    ,   "tsrnev"
    ,   "sdttsa"
    ,   "rasrtv"
    ,   "nssdts"
    ,   "ntnada"
    ,   "svetve"
    ,   "tesnvt"
    ,   "vntsnd"
    ,   "vrdear"
    ,   "dvrsen"
    ,   "enarar"
    ]