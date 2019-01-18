module Advent2016_5 where

import Data.List
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Internal

type DoorId = String

data Door = Door { doorId :: DoorId, indexes :: [Int] }


--getHashes :: Door -> []

stringToHashString :: String -> String
stringToHashString = show . md5 . packChars

areFirstNcharsZero :: Int -> String -> Bool
areFirstNcharsZero n str = all(=='0') $ take n str

areFirstFiveCharsZero :: String -> Bool
areFirstFiveCharsZero = areFirstNcharsZero 5

areFirstFiveCharsZeroAndSixthInRange :: String -> Bool
areFirstFiveCharsZeroAndSixthInRange str = areFirstFiveCharsZero str && (isInRange sixthChar)
    where sixthChar = str !! 5
          isInRange x = elem x (map (head . show) [0..7])

hashDoor :: Door -> String
hashDoor (Door doorId (i:is)) = hashDoorWithIndex doorId i

instance Show Door where
    show d = "door id: " <> doorId d <> ", has hash: " <> hashDoor d <> ", at index: " <> (show . head) (indexes d) <> "\n"

hashDoorWithIndex :: DoorId -> Int -> String
hashDoorWithIndex doorId i = stringToHashString $ doorId <> (show i)

recurseDoorHashUntil :: (String -> Bool) -> Door -> Door
recurseDoorHashUntil func d@(Door dId (i:is))
    | func $ hashDoor d = d
    | otherwise = recurseDoorHashUntil func (Door dId is)

recurseDoorHashUntilFiveLeadingZeros = recurseDoorHashUntil areFirstFiveCharsZero

recurseDoorHashUntilFiveLeadingZerosAndSixthInRange = recurseDoorHashUntil areFirstFiveCharsZeroAndSixthInRange

getDoorPassword :: (Door -> Door) -> Door -> [Door]
getDoorPassword func d =
    let
        thisDoor = func d
        nextDoor = thisDoor { indexes = tail $ indexes thisDoor }
        passwordDoors = thisDoor : getDoorPassword func nextDoor
    in
        take 8 passwordDoors

regularPassword :: Door -> [Door]
regularPassword = getDoorPassword recurseDoorHashUntilFiveLeadingZeros

specialPassword :: Door -> [Door]
specialPassword = getDoorPassword recurseDoorHashUntilFiveLeadingZerosAndSixthInRange

getSixthElem :: [a] -> a
getSixthElem = (head . drop 5)

getSixSevenPair :: [a] -> (a, a)
getSixSevenPair ls = (ls !! 5, ls !! 6)



initialDoor = (Door "cxdnnyjw" [0..])


doorPasswordString doors = map (getSixthElem . hashDoor) doors

firstAnswer = doorPasswordString (regularPassword initialDoor)

doorSortedPassword doors = sort $ map (getSixSevenPair . hashDoor) doors

secondAnswer = doorSortedPassword (specialPassword initialDoor) --boo you need a map to get rid of duplicates
