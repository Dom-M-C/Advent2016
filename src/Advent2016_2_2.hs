module Advent2016_2_2 
    (   --firstAnswer
        secondAnswer
    ) where

newtype KeyPosition = P Int deriving(Show, Eq, Ord)

instance Bounded KeyPosition where
    minBound = P 0
    maxBound = P 2

instance Semigroup KeyPosition where
    P a <> P b = P (a + b)

instance Monoid KeyPosition where
    mempty = P 0    

data KeypadButton = K { xpos :: KeyPosition, ypos :: KeyPosition } deriving (Ord, Eq, Bounded)

instance Show KeypadButton where
    show (K (P 0) (P 0))    = "2"
    show (K (P 1) (P 0))    = "3"
    show (K (P 2) (P 0))    = "4"
    show (K (P 0) (P 1))    = "6"
    show (K (P 1) (P 1))    = "7"
    show (K (P 2) (P 1))    = "8"
    show (K (P 0) (P 2))    = "A"
    show (K (P 1) (P 2))    = "B"
    show (K (P 2) (P 2))    = "C"
    show (K (P 1) (P (-1))) = "1"
    show (K (P (-1)) (P 1)) = "5"
    show (K (P 3) (P 1))    = "9"
    show (K (P 1) (P 3))    = "D"

class KeyMove a where
    up :: a -> a
    left :: a -> a
    down :: a -> a
    right :: a -> a

instance KeyMove KeypadButton where
    up (K (P 1) (P 0)) = (K (P 1) (P (-1))) --if up on "3" return "1"
    up k
        | (ypos k) == (minBound :: KeyPosition) = k
        | otherwise = k { ypos = (ypos k) <> (P (-1)) }
    left (K (P 0) (P 1)) = (K (P (-1)) (P 1)) --if left on 6 return 5
    left k
        | (xpos k) == (minBound :: KeyPosition) = k
        | otherwise = k { xpos = (xpos k) <> (P (-1)) }
    down (K (P 1) (P 2)) = (K (P 1) (P 3)) --if down on B return D
    down k
        | (ypos k) == (maxBound :: KeyPosition) = k
        | otherwise = k { ypos = (ypos k) <> (P 1) }
    right (K (P 2) (P 1)) = (K (P 3) (P 1)) --if right on 8 return 9        
    right k
        | (xpos k) == (maxBound :: KeyPosition) = k
        | otherwise = k { xpos = (xpos k) <> (P 1) }

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

initialButton = (K (P (-1)) (P 1)) -- "5"  
    
moveButton :: KeypadButton -> Move -> KeypadButton
--if position is D can only move up
moveButton d@(K (P 1) (P 3)) m 
    | m == U = up d
    | otherwise = d
--if position is 9 can only move left
moveButton nine@(K (P 3) (P 1)) m
    | m == L = left nine
    | otherwise = nine
--if position is 1 can only move down
moveButton one@(K (P 1) (P (-1))) m 
    | m == D = down one
    | otherwise = one
--if position is 5 can only move right
moveButton five@(K (P (-1)) (P 1)) m 
    | m == R = right five
    | otherwise = five    

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

secondAnswer = getKeys initialButton moves


   
--func = N [L]

    
