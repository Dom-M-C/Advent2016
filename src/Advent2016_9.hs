{-# LANGUAGE OverloadedStrings #-}

module Advent2016_9 where



import qualified Data.Text as T

type DataLength = Int
type RepetitionTimes = Int

data Marker = Marker DataLength RepetitionTimes deriving Show

type CompressedText = T.Text
type DecompressedString = String



processMarker :: Marker -> CompressedText -> (DecompressedString, CompressedText)
processMarker = undefined

parseInput :: T.Text -> [(Marker, CompressedText)]
parseInput txt = 
    let
        (mark, rest) = T.breakOn  ")" txt
        (dLength, rTimes) = (\(x, y) -> (T.unpack x, T.unpack (T.drop 1 y)) ) . T.breakOn "x" . T.drop 1 $ mark
        marker = Marker (read dLength :: DataLength) (read rTimes :: RepetitionTimes)
    in
        [(marker, "")]


parseMarker :: T.Text -> (T.Text)
parseMarker = undefined

testInput :: T.Text
testInput = "(6x9)JUORKH(10x13)LNWIKDMACM(126x14)(21x8)QLKUJNVVZIQGGFCJZMPHK(2x1)ZH"