module Advent2016_5 
    (firstAnswer
    ,secondAnswer
    )where

import Data.List
import qualified Data.Map as Map
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Internal

type DoorId = String

data Door = Door { doorId :: DoorId, indexes :: [Int] }

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

recurseDoorHashUntil :: (String -> Bool) -> Door -> Door
recurseDoorHashUntil func d@(Door dId (i:is))
    | func $ hashDoor d = d
    | otherwise = recurseDoorHashUntil func (Door dId is)

recurseDoorHashUntilFiveLeadingZeros = recurseDoorHashUntil areFirstFiveCharsZero

getDoorPassword :: (Door -> Door) -> Door -> [Door]
getDoorPassword func d =
    let
        thisDoor = func d
        nextDoor = thisDoor { indexes = tail $ indexes thisDoor }
        passwordDoors = thisDoor : getDoorPassword func nextDoor
    in
        passwordDoors

regularPassword :: Door -> [Door]
regularPassword = take 8 . getDoorPassword recurseDoorHashUntilFiveLeadingZeros

getSixthElem :: [a] -> a
getSixthElem = (head . drop 5)

getSixSevenPair :: [a] -> (a, a)
getSixSevenPair ls = (ls !! 5, ls !! 6)

getPosAnswerPair = (getSixSevenPair . hashDoor . head )

initialDoor = (Door "cxdnnyjw" [0..])

doorPasswordString doors = map (getSixthElem . hashDoor) doors

firstAnswer = doorPasswordString (regularPassword initialDoor)

areFirstFiveCharsZeroAndGetSixSevenPair :: String -> Maybe (Int, Char)
areFirstFiveCharsZeroAndGetSixSevenPair str
    | areFirstFiveCharsZero str && isSixthInRange = Just (asInt, str !! 6)
    | otherwise = Nothing
    where sixthChar = str !! 5
          isInRange x = elem x ['0'..'7']
          isSixthInRange = isInRange sixthChar
          asInt = read [sixthChar] :: Int

getValidDoorPair :: Door -> Maybe (Int, Char)
getValidDoorPair = areFirstFiveCharsZeroAndGetSixSevenPair . hashDoor

--recurseDoorHashUntilFiveLeadingZerosAndSixthInRange :: Door -> Door
recurseDoorHashUntilFiveLeadingZerosAndSixthInRange =
    let
        doors = getDoorPassword recurseDoorHashUntilFiveLeadingZeros
        doorPair = map (\x -> (getValidDoorPair x, x)) (doors initialDoor)
        pairs = map fst doorPair 
    in
        createMap Map.empty pairs

createMap :: Map.Map Int Char -> [Maybe (Int, Char)] -> Map.Map Int Char
createMap m (Nothing:ts) = createMap m ts
createMap m (Just (i, c):ts)
    | Map.size m == 8 = m
    | Map.lookup i m == Nothing = createMap (Map.insert i c m) ts
    | otherwise = createMap m ts

secondAnswer :: [Char]
secondAnswer = map snd $ Map.toList recurseDoorHashUntilFiveLeadingZerosAndSixthInRange

