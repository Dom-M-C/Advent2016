module Advent2016_5 where 


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

hashDoor :: Door -> String
hashDoor (Door doorId (i:is)) = hashDoorWithIndex doorId i

instance Show Door where
    show d = "door id: " <> doorId d <> ", has hash: " <> hashDoor d <> ", at index: " <> (show . head) (indexes d) <> "\n"

hashDoorWithIndex :: DoorId -> Int -> String
hashDoorWithIndex doorId i = stringToHashString $ doorId <> (show i)

recurseDoorHashUntilFiveLeadingZeros :: Door -> Door
recurseDoorHashUntilFiveLeadingZeros d@(Door dId (i:is))
    | areFirstFiveCharsZero $ hashDoor d = d
    | otherwise = recurseDoorHashUntilFiveLeadingZeros (Door dId is)

getDoorPassword :: Door -> [Door]
getDoorPassword d = 
    let
        thisDoor = recurseDoorHashUntilFiveLeadingZeros d
        nextDoor = thisDoor { indexes = tail $ indexes thisDoor }
        passwordDoors = thisDoor : getDoorPassword nextDoor
    in
        take 8 passwordDoors



getSixthElem :: [a] -> a
getSixthElem = (head . drop 5)

getSixSevenPair = undefined



initialDoor = (Door "cxdnnyjw" [0..])


doorPasswordString doors = map (getSixthElem . hashDoor) doors

firstAnswer = doorPasswordString $ getDoorPassword initialDoor 