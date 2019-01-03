module Advent2016_2 where 


data KeypadButtons = K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 deriving (Show, Enum, Ord, Eq, Bounded)
data Move = None | Up | Left | Down | Right deriving(Show, Enum, Eq)

class KeyMove a where
    

--instance 