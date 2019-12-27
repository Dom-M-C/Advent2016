{-# LANGUAGE OverloadedStrings #-}

module Advent2019_2 where 

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map as Map


input = do
    opcodeText <- TIO.readFile "src\\2019_2_input.txt"
    let opcodes = T.splitOn "," opcodeText
    return $ map (\x -> (read . T.unpack) x :: Int) opcodes


inputTuples = zip [0..] <$> input

inputMap = Map.fromList <$> inputTuples

data Opcode = Terminate
    | AddCode
    {   paramIndexes :: (Int, Int)
    ,   outputIndex :: Int
    }
    | MultiplyCode
    {   paramIndexes :: (Int, Int)
    ,   outputIndex :: Int
    } deriving Show

traverseInput (99:_) = Terminate:[]
traverseInput (1:i1:i2:out:rest) = AddCode (i1, i2) out : traverseInput rest
traverseInput (2:i1:i2:out:rest) = MultiplyCode (i1, i2) out : traverseInput rest

doOpcode :: Num a => Opcode -> Map.Map Int a -> Map.Map Int a
doOpcode Terminate map = map
doOpcode op map = setOutput op (doOperation op map) map

getFirstArg :: Map.Map Int a -> Opcode -> a
getFirstArg map = (Map.!) map . fst . paramIndexes

getSecondArg :: Map.Map Int a -> Opcode -> a
getSecondArg map = (Map.!) map . snd . paramIndexes

setOutput :: Opcode -> a -> Map.Map Int a -> Map.Map Int a
setOutput = Map.insert . outputIndex

doOperation :: Num a => Opcode -> Map.Map Int a -> a
doOperation op map = (operation op) (getFirstArg map op) (getSecondArg map op)
    where
        operation (AddCode _ _) = (+)
        operation (MultiplyCode _ _) = (*)

doOpcodes :: Num a => Map.Map Int a -> [Opcode] -> Map.Map Int a
doOpcodes map (op:codes) = doOpcodes (doOpcode op map) codes
doOpcodes map [] = map

partOne = doOpcodes <$> inputMap <*> (traverseInput <$> input)
partTwo = undefined <$> input
 