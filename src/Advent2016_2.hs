module Advent2016_2 
    (   firstAnswer
    --,   secondAnswer
    ) where

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

data KeypadButton = K { xpos :: KeyPosition, ypos :: KeyPosition } deriving (Ord, Eq, Bounded)

instance Show KeypadButton where
    show (K 0 0) = "1"
    show (K 1 0) = "2"
    show (K 2 0) = "3"
    show (K 0 1) = "4"
    show (K 1 1) = "5"
    show (K 2 1) = "6"
    show (K 0 2) = "7"
    show (K 1 2) = "8"
    show (K 2 2) = "9"

class KeyMove a where
    up :: a -> a
    left :: a -> a
    down :: a -> a
    right :: a -> a

instance KeyMove KeypadButton where
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
        | (xpos k) == (maxBound :: KeyPosition) = k
        | otherwise = k { xpos = (xpos k) + 1 }

data Move = None | U | L | D | R deriving(Show, Enum, Eq)

class ParseMove a where
    readMove :: Char -> a
    readMoves :: String -> [a]

instance ParseMove Move where
    readMove 'U' = U
    readMove 'L' = L
    readMove 'D' = D
    readMove 'R' = R
    readMoves str = map readMove str

initialButton = K 1 1 -- "5"  
    
moveButton :: KeypadButton -> Move -> KeypadButton
moveButton kb U = up kb
moveButton kb L = left kb
moveButton kb D = down kb
moveButton kb R = right kb

moveButtons :: KeypadButton -> [Move] -> KeypadButton
moveButtons key ms = foldl moveButton key ms

    
buttons = 
    [   "UUURRRRULRDLRDRRDURDDDLLDLLLULDUDDLDLULUURULRLDLRRLLLRRDRRLDDLLULUDUDDLRDRDUURDLURUURLRULLDDURULRRURDUURLULUUUURDDDDUUDLULRULLLRLLRRRURDLLRLLRRRUURULRDRUUDDDDDLLLRURRURRUURDUURDDRDLULRRLLLDRRRLURRLLURLDRRDDLDLRRLLRDRLLLLDLULDLRRDRRLDDURLULLUDLUDRRDRRLRLULURDRLRLUUUDLRLDLLLURDUDULULDDRUUURLLLDLLDDUDDRURURUDDLUULRDRRRRLDRDDURLUDURDULLDLUDLULDRLRLLRLLLLRURDURLLDRRDRLRUUUUULLLRUDURUDLLLUDLLLLRDLDRDUDRURLUDDUDDURLUUUUDDLLUDLULLLLLDUDLLRLRRDDDULULRLDRLLULDLUDLLURULRDDUURULRDLDLDLRL"
    ,   "URUUURDULUDLUUUUDDRRRDRRRLDUDLRDRRDRDDLRUULDLLDUDULLLRLDRDRRLDLDLUUDRUULDUDULDUDURURDDURULDLURULRLULDUDDUULDLLLDDURDDRDDURUULUUDRLDDULDRRRRDURRUDLLLURDDDLRULLRDDRDDDDLUUDRDUULRRRRURULDDDLDDRDRRUDRRURUDRDDLDRRRLLURURUULUUDRDULLDRLRDRRDDURDUDLDRLUDRURDURURULDUUURDUULRRRRRUDLLULDDDRLULDDULUDRRRDDRUDRRDLDLRUULLLLRRDRRLUDRUULRDUDRDRRRDDRLLRUUDRLLLUDUDLULUUDULDRRRRDDRURULDULLURDLLLDUUDLLUDRLDURRRLDDDURUDUDURRULDD"
    ,   "LRUDDULLLULRLUDUDUDRLLUUUULLUDLUUUUDULLUURDLLRDUDLRUDRUDDURURRURUDLLLRLDLUDRRRRRRDLUURLRDDDULRRUDRULRDRDDUULRDDLDULDRRRDDLURRURLLLRURDULLRUUUDDUDUURLRLDDUURLRDRRLURLDRLLUUURDRUUDUUUDRLURUUUDLDRRLRLLRRUURULLLRLLDLLLDULDDLDULDLDDRUDURDDURDUDURDLLLRRDDLULLLUDURLUDDLDLUUDRDRUDUUDLLDDLLLLDRDULRDLDULLRUDDUULDUDLDDDRUURLDRRLURRDDRUUDRUDLLDLULLULUDUDURDDRLRDLRLDRLDDRULLLRUDULDRLRLRULLRLLRRRLLRRRDDRULRUURRLLLRULDLUDRRDDLLLUDDUDDDLURLUDRDLURUUDLLDLULURRLLDURUDDDDRLULRDDLRLDDLRLLDDRRLRDUDUUULRRLRULUDURDUDRLRLRUDUDLLRRRRLRRUDUL"
    ,   "RULLLLUUUDLLDLLRULLRURRULDDRDLUULDRLLRUDLLRRLRDURLLDUUUUURUUURDLUURRLDDDLRRRRLRULDUDDLURDRRUUDLRRRDLDDUDUDDRUDURURLDULLDLULDLLUDLULRDRLLURRLLDURLDLRDLULUDDULDLDDDDDUURRDRURLDLDULLURDLLDDLLUDLDLDRLRLDLRDRLDLRRUUDRURLUUUDLURUULDUDRDULLDURUDLUUURRRLLDUDUDDUUULLLRUULDLURUDDRLUDRDDLDLLUDUDRRRDDUUULUULLLRLLUURDUUDRUUULULLDLDRUUDURLLUULRLDLUURLLUUDRURDDRLURULDUDUUDRRUUURDULRLDUUDDRURURDRRULDDDRLUDLLUUDURRRLDLRLRDRURLURLLLRLDDLRRLDLDDURDUUDRDRRLDRLULDRLURUUUDDRLLLDDLDURLLLLDRDLDRRUDULURRLULRDRLLUULLRLRDRLLULUURRUDRUDDDLLDURURLURRRDLLDRDLUDRULULULRLDLRRRUUDLULDURLRDRLULRUUURRDDLRUURUDRURUDURURDD"
    ,   "DURRDLLLDDLLDLLRLULULLRDLDRRDDRDLRULURRDUUDDRLLDDLDRRLRDUDRULDLRURDUUDRDDLLDRRDRUDUDULLDDDDLDRRRLRLRDRDLURRDDLDDDUUDRDRLLLDLUDDDLUULRDRLLLRLLUULUDDDRLDUUUURULRDDURRDRLUURLUDRLRLLLDDLRDDUULRRRRURDLDDDRLDLDRRLLDRDDUDDUURDLDUUDRDLDLDDULULUDDLRDDRLRLDDLUDLLDRLUDUDDRULLRLDLLRULRUURDDRDRDRURDRRLRDLLUDDRRDRRLDDULLLDLUDRRUDLDULDRURRDURLURRLDLRDLRUDLULUDDRULRLLDUURULURULURRLURRUULRULRRRLRDLULRLRLUDURDDRUUURDRLLRRRDDLDRRRULLDLRDRULDRRLRRDLUDDRDDDUUURRLULLDRRUULULLRRRRLDDRDDLUURLLUDLLDUDLULUULUDLLUUURRRUDDDRLLLRDRUUDUUURDRULURRLRDLLUURLRDURULDRRUDURRDDLDRLDRUUDRLLUDLRRU"
    ]
    
moves = map (\x -> readMoves x :: [Move]) buttons

getKeys kb (m:[]) = [moveButtons kb m]
getKeys kb (m:ms) = moveButtons kb m : getKeys (moveButtons kb m) ms 

firstAnswer = getKeys initialButton moves

