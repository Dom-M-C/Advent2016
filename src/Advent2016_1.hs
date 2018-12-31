module Advent2016_1 where

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
data Move = L { steps :: Int } | R { steps :: Int } deriving(Show, Read)

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

writeCoordinates :: [Position] -> [(Longitude, Latitude, Int )]
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

updateLongLat :: Direction -> Position
updateLongLat d = case d of
    South -> P 0 (-1) South
    West -> P (-1) 0 West 
    North -> P 0 1 North
    East -> P 1 0 East


move :: Position -> Move -> [Position]
move p m 
    | steps m < 1 = []
    | otherwise =
    let
        dir = facing p
        newDir = updateFacing dir m
        stepValue = updateLongLat newDir
        newPos = p { x = x p + x stepValue
            ,  y = y p + y stepValue
            ,  facing = newDir
            } 
    in newPos : move newPos ( steps m (-1) )
    
getMoves :: String -> [Move]
getMoves = 
    let 
        cleanString = \str -> removeCommas str
        moveList = \str -> words str
    in \str -> map parseMove (moveList $ cleanString str) 

removeCommas :: String -> String
removeCommas xs = filter (\x -> x /= ',') xs

parseMove :: String -> Move
parseMove ('L':xs) = L (read xs::Int)
parseMove ('R':xs) = R (read xs::Int)

executeMoves :: [Move] -> Position
executeMoves moves = foldl move initialPosition moves

gottenMoves = getMoves "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3"

---PART TWO

moved = move initialPosition


--mapped = \moves -> map (moved) moves
--summed = \positions -> apply sumPositions positions

apply :: (Position -> Move -> Position) -> Position -> [Move] -> [Position]
apply f pos (x:[]) = [f pos x]
apply f pos (x:xs) = (f pos x) : apply f (f pos x) xs


