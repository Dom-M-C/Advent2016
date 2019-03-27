{-# LANGUAGE OverloadedStrings #-}

module Advent2016_9 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type DataLength = Int
type RepetitionTimes = Int

data Marker = Marker
    {   dataLength :: DataLength
    ,   repetitionTimes :: RepetitionTimes
    }

instance Show Marker where
    show (Marker dl rt) = mconcat [show dl, "x", show rt]

instance Semigroup Marker where
    (<>) (Marker dl1 rt1) (Marker dl2 rt2) = Marker (dl2 - dl1) (rt1 * rt2)

instance Monoid Marker where
    mempty = Marker 0 1

type CompressedText = T.Text
type DecompressedString = String

data PreprocessedText = PreprocessedText
    {   marker :: Marker
    ,   compressedText :: CompressedText } 
    | Straggler
    {   stragglers :: CompressedText }

instance Show PreprocessedText where
    show (PreprocessedText m ct) = mconcat [show m, " ", T.unpack ct]
    show (Straggler s) = show s

--instance Semigroup PreprocessedText where
--    (<>) (PreprocessedText m1 ct1) (PreprocessedText m2 ct2) = PreprocessedText (m1 <> m2) (ct1 <> ct2)
--
--instance Monoid PreprocessedText where
--    mempty = PreprocessedText (mempty :: Marker) (mempty :: T.Text)

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

--parseInput'' :: T.Text -> [(PreprocessedText, T.Text)]
--parseInput'' "" = ([], "")



expandFirstMark :: PreprocessedText -> DecompressedString
expandFirstMark (PreprocessedText mark txt) = expandedText txt <> leftoverText txt
    where
        expandedText = T.unpack . T.replicate (repetitionTimes mark) . T.take (dataLength mark)
        leftoverText = T.unpack . T.drop (dataLength mark)

processFirstMarks :: T.Text -> [DecompressedString]
processFirstMarks = map expandFirstMark . parseInput

extractMarks :: CompressedText -> [[CompressedText]]
extractMarks ct = map (T.splitOn ")") . T.splitOn "(" $ ct

preProcessText :: CompressedText -> [PreprocessedText]
preProcessText txt = map (\(x, y) -> PreprocessedText x y) $ zip marks txts
    where
        base = extractMarks $ txt
        marks = map parseMarker . map head $ base
        txts = map last base

collapsePreProcessed :: [PreprocessedText] -> [PreprocessedText]
collapsePreProcessed [] = []

collapsePreProcessed (PreprocessedText m1 "" : PreprocessedText m2 t2 : pts) =
    collapsePreProcessed (PreprocessedText (m1 <> m2) (t2) : pts)

collapsePreProcessed (PreprocessedText m1 t1 : pts) = PreprocessedText m1 t1
    : collapsePreProcessed pts

postCollapsedPreProcess = collapsePreProcessed . preProcessText

testInput :: T.Text
testInput = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"

testInput' :: T.Text
testInput' = "(27x12)(20x12)(13x14)(7x10)(1x12)A"

inputIO :: IO T.Text
inputIO = do
    file <- TIO.readFile ".\\src\\input9.txt"
    return file

firstAnswer = (sum . map length . processFirstMarks) <$> inputIO

secondAnswer = (length . mconcat . map expandFirstMark . postCollapsedPreProcess) $ testInput -- <$> inputIO
