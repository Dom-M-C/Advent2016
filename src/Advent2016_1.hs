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
data Move = L Steps | R Steps deriving(Show, Read)

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

getSteps :: Move -> Steps
getSteps (L s) = s
getSteps (R s) = s

updateLongLat :: Direction -> Move -> Position
updateLongLat d m = case d of
    South -> P 0 (-s) South
    West -> P (-s) 0 West 
    North -> P 0 s North
    East -> P s 0 East
    where s = getSteps m

sumPositions :: Position -> Position -> Position    
sumPositions current new = 
    let
        newLat = x current + x new
        newLong = y current + y new
        dir = facing new
    in P newLat newLong dir

move :: Position -> Move -> Position
move p m = 
    let
        dir = facing p
        newDir = updateFacing dir m
        newPos = updateLongLat newDir m
    in sumPositions p newPos
    
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

