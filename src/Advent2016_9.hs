{-# LANGUAGE OverloadedStrings #-}

module Advent2016_9 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe

type DataLength = Int
type RepetitionTimes = Int
type CompressedText = T.Text
type DecompressedString = String

data Marker = Marker
    {   dataLength :: DataLength
    ,   repetitionTimes :: RepetitionTimes } deriving Eq

instance Show Marker where
    show (Marker dl rt) = mconcat [show dl, "x", show rt]

instance Semigroup Marker where
    (<>) (Marker dl1 rt1) (Marker dl2 rt2) = Marker (min dl2 dl1) (rt1 * rt2)

instance Monoid Marker where
    mempty = Marker 0 1

data PreprocessedText = PreprocessedText
    {   marker :: Marker
    ,   compressedText :: CompressedText }
    | Straggler
    {   stragglers :: CompressedText }

instance Show PreprocessedText where
    show (PreprocessedText m ct) = mconcat [show m, " ", T.unpack ct]
    show (Straggler s) = show s

readText :: Read a => T.Text -> a
readText = read . T.unpack

parseMarker :: T.Text -> Marker
parseMarker "" = mempty
parseMarker txt = Marker (readText dLength :: DataLength) (readText rTimes :: RepetitionTimes)
    where
        (dLength, rTimes) = (\(x, y) -> (x, T.drop 1 $ y) ) . T.breakOn "x" $ txt

parseInput :: T.Text -> [PreprocessedText]
parseInput "" = []
parseInput txt =
    let
        (mark, rest) = (\(x,y) -> (T.drop 1 x, T.drop 1 y)) . T.breakOn  ")" $ txt
        marker = parseMarker mark
        next m = T.drop (dataLength m) rest
        this m = T.take (dataLength m) rest
    in
        (PreprocessedText marker (this marker)) : parseInput (next marker)

parseInput' :: T.Text -> [PreprocessedText]
parseInput' "" = []
parseInput' txt
    | T.head txt == ')' = parseInput' . T.drop 1 $ txt
    | T.head txt /= '(' = Straggler straggles : parseInput' (mark <> ")" <> rest)
    | otherwise = (PreprocessedText marker (this marker)) : parseInput' (next marker)
    where
        (lead, rest) = (\(x,y) -> (x, T.drop 1 y)) . T.breakOn  ")" $ txt
        marker = parseMarker . T.drop 1 $ lead
        next m = T.drop (dataLength m) rest
        this m = T.take (dataLength m) rest
        (straggles, mark) = T.breakOn "(" lead

countMultiMark :: PreprocessedText -> Int
countMultiMark (Straggler ct) = T.length ct
countMultiMark (PreprocessedText m ct) = sum . map expandedInt . processPairs $ pairs -- zip marks txt --(PreprocessedText m ct) = marks
    where
        pairs = filter (\(x, y) -> not (isNothing x && y == (Just "")) ) . extractMarks $ ct
        expandedInt (PreprocessedText a b) = T.length b * (repetitionTimes (m<>a))

processPairs :: [(Maybe Marker, Maybe CompressedText)] -> [PreprocessedText]
processPairs [] = []
processPairs ((Just mark, Just txt) :xs) = PreprocessedText mark txt : processPairs xs
processPairs ((Just mark, Nothing)  :(Just mark2, Just txt2):xs) = processPairs ((Just (mark <> mark2), Just txt2):xs) 
processPairs ((Just mark, Nothing)  :(Nothing, Just txt2):xs) = processPairs ((Just (mark), Just txt2):xs) 
processPairs ((Nothing, Just txt)   :(Just mark2, Just txt2):xs) = processPairs ((Just (mempty), Just txt):xs) 
processPairs ((Nothing, Just txt)   :(Nothing, Just txt2):xs) = processPairs ((Just (mempty), Just txt):xs) 
processPairs ((Nothing, Just txt2)  :xs) = processPairs ((Just (mempty), Just txt2):xs) 

foldText :: [PreprocessedText] -> PreprocessedText
foldText xs = foldl foldf (PreprocessedText mempty "") xs
    where foldf (PreprocessedText x y) (PreprocessedText a b) = PreprocessedText (x<>a) ""

expandFirstMark :: PreprocessedText -> DecompressedString
expandFirstMark (PreprocessedText mark txt) = expandedText txt <> leftoverText txt
    where
        expandedText = T.unpack . T.replicate (repetitionTimes mark) . T.take (dataLength mark)
        leftoverText = T.unpack . T.drop (dataLength mark)

processFirstMarks :: T.Text -> [DecompressedString]
processFirstMarks = map expandFirstMark . parseInput

extractMarks :: CompressedText -> [(Maybe Marker, Maybe T.Text)]
extractMarks ct = map (\x -> if T.isInfixOf "x" x then (Just (parseMarker x), Nothing) else (Just mempty, Just x)) .  mconcat $ marks
    where
        marks = map (T.splitOn ")") . T.splitOn "(" $ ct

testInput :: T.Text
testInput = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"

testInput' :: T.Text
testInput' = "(27x12)(20x12)(13x14)(7x10)(1x12)A"

inputIO :: IO T.Text
inputIO = do
    file <- TIO.readFile "src\\input9.txt"
    return file

firstAnswer = (sum . map length . processFirstMarks) <$> inputIO

--secondAnswer = (sum . map countMultiMark  . parseInput') <$> inputIO
