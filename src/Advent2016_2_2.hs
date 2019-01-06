module Advent2016_2_2
    (--   firstAnswer
        secondAnswer
    ) where


import Advent2016_2

newtype DiamondPosition = DiamondPosition KeyPosition deriving(Show)

newtype DiamondKeypadButton = DiamondKeypadButton KeypadButton


instance Bounded DiamondPosition where
    minBound = DiamondPosition 0
    maxBound = DiamondPosition 5
{-
instance Show Advent2016_2.KeypadButton where
    show (K 0 0) = "1"
    show (K 1 0) = "2"
    show (K 2 0) = "3"
    show (K 0 1) = "4"
    show (K 1 1) = "5"
    show (K 2 1) = "6"
    show (K 0 2) = "7"
    show (K 1 2) = "8"
    show (K 2 2) = "9"
-}
secondAnswer = K 0 0
