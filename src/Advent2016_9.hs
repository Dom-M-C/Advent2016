{-# LANGUAGE OverloadedStrings #-}

module Advent2016_9 where

import qualified Data.Text as T

type DataLength = Int
type RepetitionTimes = Int

data Marker = Marker
    {   dataLength :: DataLength
    ,   repetitionTimes :: RepetitionTimes
    } deriving Show

type CompressedText = T.Text
type DecompressedString = String
type PreprocessedText = (Marker, CompressedText)

readText :: Read a => T.Text -> a
readText = read . T.unpack

parseMarker :: T.Text -> Marker
parseMarker txt = Marker (readText dLength :: DataLength) (readText rTimes :: RepetitionTimes)
    where
        (dLength, rTimes) = (\(x, y) -> (x, T.drop 1 $ y) ) . T.breakOn "x" . T.drop 1 $ txt

parseInput :: T.Text -> [PreprocessedText]
parseInput "" = []
parseInput txt = 
    let
        (mark, rest) = (\(x,y) -> (x, T.drop 1 y)) . T.breakOn  ")" $ txt
        marker = parseMarker mark
        next m = T.drop (dataLength m) rest
        this m = T.take (dataLength m) rest
    in
        (marker, this marker) : parseInput (next marker)

expandMarks :: PreprocessedText -> DecompressedString
expandMarks = undefined

processText :: T.Text -> [DecompressedString]
processText = map expandMarks . parseInput 

testInput :: T.Text
testInput = "(6x9)JUORKH(10x13)LNWIKDMACM(126x14)(21x8)QLKUJNVVZIQGGFCJZMPHK(2x1)ZH"


