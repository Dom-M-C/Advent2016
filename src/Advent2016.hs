module Advent2016 where


data TurnDirection = L | R
data CompassDirection = North | East | South | West deriving(Show, Enum, Bounded)

turn :: CompassDirection -> TurnDirection -> CompassDirection
turn L North = West
turn R West = North
turn d L = pred d
turn d R = succ d

turnLeft d = turn d L
turnRight d = turn d R

processTurns = foldl (turn) 
