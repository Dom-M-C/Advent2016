{-# LANGUAGE OverloadedStrings #-}

module Advent2019_2 where 

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

input = do
    opcodeText <- TIO.readFile "src\\2019_2_input.txt"
    let opcodes = T.splitOn "," opcodeText
    let intcodes = map (\x -> (read . T.unpack) x :: Int) opcodes
    return $ updateInput intcodes


inputTuples = zip [0..] <$> input

inputMap = Map.fromList <$> inputTuples

inputVector = V.fromList <$> input

type Address = Int

data Opcode = Terminate
    | AddCode
    {   paramAddresses :: (Address, Address)
    ,   outputAddress :: Address
    }
    | MultiplyCode
    {   paramAddresses :: (Address, Address)
    ,   outputAddress :: Address
    } deriving Show 

traverseInput (99:_) = Terminate:[]
traverseInput (1:i1:i2:out:rest) = AddCode (i1, i2) out : traverseInput rest
traverseInput (2:i1:i2:out:rest) = MultiplyCode (i1, i2) out : traverseInput rest

doOpcode :: Num a => Opcode -> Map Address a -> Map Address a
doOpcode Terminate map = map
doOpcode op map = setOutput op (doOperation op map) map

getFirstArg :: Map Address a -> Opcode -> a
getFirstArg map = (Map.!) map . fst . paramAddresses

getSecondArg :: Map Address a -> Opcode -> a
getSecondArg map = (Map.!) map . snd . paramAddresses

setOutput :: Opcode -> a -> Map Address a -> Map Address a
setOutput = Map.insert . outputAddress

doOperation :: Num a => Opcode -> Map Address a -> a
doOperation op map = (operation op) (getFirstArg map op) (getSecondArg map op)
    where
        operation (AddCode _ _) = (+)
        operation (MultiplyCode _ _) = (*)

doOpcodes :: Num a => Map Address a -> [Opcode] -> Map Address a
doOpcodes map (op:codes) = doOpcodes (doOpcode op map) codes
doOpcodes map [] = map

updateInput (x:_:_:rest) = x:12:2:rest

partOne = doOpcodes <$> inputMap <*> (traverseInput <$> input)
partTwo = undefined <$> input



type Memory = Vector Instruction

data InstructionParameter = BinaryInstruction
    {   inputParameters :: (Address, Address)
    ,   outputParameter :: Address
    }

data Instruction = TerminateInstruction
    | AddInstruction InstructionParameter
    | MultiplyInstruction InstructionParameter

data IntcodeProgram = IntcodeProgram
    {   instructionPointer :: Instruction
    ,   programMemory :: Memory
    }

initializeProgram vec = IntcodeProgram (vec ! 0) vec
