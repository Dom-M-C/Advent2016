module Advent2019_1 where 

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text.Read

type Mass = Int
type Fuel = Int

fuelRequired :: Mass -> Fuel
fuelRequired m = (m `div` 3) - 2

sumOfFuelRequired :: [Mass] -> Fuel
sumOfFuelRequired = sum . map fuelRequired

input = do
    masses <- TIO.readFile "src\\2019_1_input.txt"
    let converted = map (\x -> (read . T.unpack) x :: Mass) (T.lines masses)
    return converted

partOne = sumOfFuelRequired <$> input


recursiveFuel :: (Mass, Mass) -> Fuel
recursiveFuel (m, t)
    | fuelNeeded <= 0 = t
    | otherwise = recursiveFuel (fuelNeeded, t + fuelNeeded)
    where 
        fuelNeeded = fuelRequired m 

sumOfRecursiveFuelRequired :: [Mass] -> Fuel
sumOfRecursiveFuelRequired = sum . map (\x -> recursiveFuel (x, 0))


partTwo = sumOfRecursiveFuelRequired <$> input
