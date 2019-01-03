module Advent2016_2 where 

newtype KeyPosition = KeyPosition Int deriving(Show, Eq, Ord)

instance Bounded KeyPosition where
    minBound = KeyPosition 0
    maxBound = KeyPosition 2

instance Num KeyPosition where
    KeyPosition a + KeyPosition b = KeyPosition (a + b)
    KeyPosition a * KeyPosition b = KeyPosition (a * b)
    abs (KeyPosition a) = KeyPosition (abs a)
    signum (KeyPosition a) = KeyPosition (signum a)
    fromInteger a = KeyPosition $ fromIntegral a
    negate (KeyPosition a) = KeyPosition (negate a)

data KeypadButtons = K { xpos :: KeyPosition, ypos :: KeyPosition } deriving (Show, Ord, Eq, Bounded)

data Move = None | Up | Left | Down | Right deriving(Show, Enum, Eq)

class KeyMove a where
    up :: a -> a
    left :: a -> a
    down :: a -> a
    right :: a -> a

instance KeyMove KeypadButtons where
    up k
        | (ypos k) == (minBound :: KeyPosition) = k
        | otherwise = k { ypos = (ypos k) - 1 }
    left k
        | (xpos k) == (minBound :: KeyPosition) = k
        | otherwise = k { xpos = (xpos k) - 1 }
    down k
        | (ypos k) == (maxBound :: KeyPosition) = k
        | otherwise = k { ypos = (ypos k) + 1 }
    right k
        | (xpos k) == (minBound :: KeyPosition) = k
        | otherwise = k { xpos = (xpos k) + 1 }
        