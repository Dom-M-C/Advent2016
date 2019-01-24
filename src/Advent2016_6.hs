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
    
countChars :: String -> Map.Map Char Int -> Map.Map Char Int
countChars [] m = m
countChars (c:cs) m =
    let
        lookupChar = Map.lookup c m 
        insertOrUpdate (Nothing) = Map.insert c 1 m
        insertOrUpdate (Just count) = Map.insert c (count + 1) m
    in
        countChars cs (insertOrUpdate lookupChar)

reduceColumnStringToMapCount :: ColumnStrings -> [Map.Map Char Int]
reduceColumnStringToMapCount (ColumnStrings m) = 
    let
        colStrings = map (snd) $ Map.toList m
        countMap = map (\x -> countChars x Map.empty)   colStrings        
    in
        countMap

mostCommonChar :: Map.Map Char Int -> Char
mostCommonChar m = fst . head . reverse . sortBy sortTupBySnd $ Map.toList m

stringOfMostCommonChars :: ColumnStrings -> String
stringOfMostCommonChars cs = map mostCommonChar $ reduceColumnStringToMapCount cs

getMaxPair tup@(x, y) = head . reverse

sortTupBySnd :: Ord b => (a, b) -> (a, b) -> Ordering
sortTupBySnd x y = compare (snd x) (snd y)

inputColumnStrings inp = buildColumnMap inp Map.empty

testAnswer = stringOfMostCommonChars $ inputColumnStrings testInput

testInput = 
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