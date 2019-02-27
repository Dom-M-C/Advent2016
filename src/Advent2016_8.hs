module Advent2016_8 where 

import Data.List
import qualified Data.Map as Map

type Position = (Int, Int)
    
data PixelState = On | Off deriving Show

type Pixels = Map.Map Position PixelState

switchState Off = On
switchState On = Off

pixelMap :: Pixels
pixelMap = Map.fromList $ map (\x -> (x, Off)) $ pure (,) <*> [0..2] <*> [0..2]


pixelMapOn :: Pixels
pixelMapOn = Map.fromList $ map (\x -> (x, On)) $ pure (,) <*> [0..2] <*> [0..1]

{-


rect :: [Pixel] -> Int -> Int -> [Pixel]
rect p x y = undefined    
-}
--rotateRow :: Pixel -> Int -> 