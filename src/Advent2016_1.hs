module Advent2016_1 
    (   firstAnswer
    ,   secondAnswer
    ) where

--https://adventofcode.com/2016/day/1

type Latitude = Int
type Longitude = Int
type Steps = Int

data Position = P {
    x :: Latitude,
    y :: Longitude,
    facing :: Direction
}

data Direction = North | East | South | West deriving(Enum, Show)
data Move = S {steps :: Steps} | L { steps :: Steps } | R { steps :: Steps } deriving(Show, Read)

instance Show Position where
    show p =
        let
        xpos = x p
        ypos = y p
        showx
            | xpos >= 0 = show xpos ++ " blocks East, "
            | xpos < 0 = show (abs xpos) ++ " blocks West, "
        showy
            | ypos >= 0 = show ypos ++ " blocks North,"
            | ypos < 0 = show (abs ypos) ++ " blocks South,"
        blocks = show $ blocksAway p
        in showx ++ showy ++ " and " ++ blocks ++ " blocks away. Currently facing: " ++ show (facing p)
    showList ps = (\_ -> show $ writeCoordinates ps) -- foldl (\p z -> show (x p, y p) ++ ", " ++ show (x z, y z))  ps

instance Eq Position where
    (==) p1 p2 = (x p1 == x p2) && (y p2 == y p1)

writeCoordinates :: [Position] -> [(Longitude, Latitude, Steps)]
writeCoordinates ps = map (\p -> (x p, y p, blocksAway p)) ps

blocksAway :: Position -> Int
blocksAway p =
    let abx = abs $ x p
        aby = abs $ y p
    in abx + aby

initialPosition :: Position
initialPosition = P 0 0 North

updateFacing :: Direction -> Move -> Direction
updateFacing North (L _) = West
updateFacing West R {} = North
updateFacing d R {} = succ d
updateFacing d (L _) = pred d
updateFacing d (S _) = d

updateLongLat :: Direction -> Steps -> Position
updateLongLat d s = case d of
    South -> P 0 (-s) South
    West -> P (-s) 0 West
    North -> P 0 s North
    East -> P s 0 East

sumPositions :: Position -> Position -> Position
sumPositions current new =
    let
        newLat = x current + x new
        newLong = y current + y new
    in current { x = newLat, y = newLong }


move :: Position -> Move -> Position
move p m =
    let
        dir = facing p
        newDir = updateFacing dir m
        newPos = updateLongLat newDir (steps m)
    in p { x = x p + x newPos
         , y = y p + y newPos  
         , facing = newDir
         }


getMoves :: String -> [Move]
getMoves =
    let
        cleanString = \str -> removeCommas str
        moveList = \str -> words str
    in \str -> map parseMove (moveList $ cleanString str)

getAllPositions :: Position -> Move -> [Position]
getAllPositions p m 
    | steps m < 1 = []
    | steps m == 1 = [newPos {facing = newDir}]
    | otherwise = newPos : getAllPositions newPos (nextMove m)
    where
        dir = facing p
        newDir = updateFacing dir m
        stepValue = updateLongLat dir 1
        nextMove m = case m of
            L s -> L (s - 1)
            R s -> R (s - 1)
        newPos = p { x = x p + x stepValue
                   , y = y p + y stepValue
                   }

getAllMoves :: Move -> [Move]
getAllMoves (S 1) = [S 1]
getAllMoves (L 1) = [L 1]
getAllMoves (R 1) = [R 1]
getAllMoves m = case m of
    R s -> R 1 : getAllMoves (S (s - 1))
    L s -> L 1 : getAllMoves (S (s - 1))
    S s -> S 1 : getAllMoves (S (s - 1))


removeCommas :: String -> String
removeCommas xs = filter (\x -> x /= ',') xs

parseMove :: String -> Move
parseMove ('L':xs) = L (read xs::Int)
parseMove ('R':xs) = R (read xs::Int)

executeMoves :: [Move] -> Position
executeMoves moves = foldl move initialPosition moves

executeAllMoves :: [Move] -> [Position]
executeAllMoves moves = apply move initialPosition moves

gottenMoves = getMoves "L2, L3, L3, L4, R1, R2, L3, R3, R3, L1, L3, R2, R3, L3, R4, R3, R3, L1, L4, R4, L2, R5, R1, L5, R1, R3, L5, R2, L2, R2, R1, L1, L3, L3, R4, R5, R4, L1, L189, L2, R2, L5, R5, R45, L3, R4, R77, L1, R1, R194, R2, L5, L3, L2, L1, R5, L3, L3, L5, L5, L5, R2, L1, L2, L3, R2, R5, R4, L2, R3, R5, L2, L2, R3, L3, L2, L1, L3, R5, R4, R3, R2, L1, R2, L5, R4, L5, L4, R4, L2, R5, L3, L2, R4, L1, L2, R2, R3, L2, L5, R1, R1, R3, R4, R1, R2, R4, R5, L3, L5, L3, L3, R5, R4, R1, L3, R1, L3, R3, R3, R3, L1, R3, R4, L5, L3, L1, L5, L4, R4, R1, L4, R3, R3, R5, R4, R3, R3, L1, L2, R1, L4, L4, L3, L4, L3, L5, R2, R4, L2"

flattenAllMoves moves = foldl (++) [] $ map getAllMoves moves

moreMoves = flattenAllMoves gottenMoves




moved = move initialPosition

returnVisited :: ([Position], [Position]) -> ([Position], [Position])
returnVisited (xs, []) = (xs, [])
returnVisited (visited, x:xs)
    | x `elem` visited = (x:visited, xs)
    | otherwise = returnVisited (x:visited, xs)

firstAnswer = executeMoves gottenMoves
secondAnswer = blocksAway . head . fst $ returnVisited ([], (executeAllMoves moreMoves))

--mapped = \moves -> map (moved) moves
--summed = \positions -> apply sumPositions positions

apply :: (Position -> Move -> Position) -> Position -> [Move] -> [Position]
apply f pos (x:[]) = [f pos x]
apply f pos (x:xs) = (f pos x) : apply f (f pos x) xs

--mapFold = map . foldl getAllMoves
