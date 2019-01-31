module Advent2016_7 where 

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import System.IO


firstAnswer = do
    ips <- getInput
    let len = length ips
    return len

getInput :: IO [IP]
getInput = do
    fileContent <- TIO.readFile "./src/input7.txt"
    let ls = T.lines fileContent 
    let ips = filter isValidIp $ map createIp ls   
    return ips


type NetSequences = [String]
type HypernetSequence = [String]

data IP = IP NetSequences HypernetSequence deriving Show

isAbba :: String -> Bool
isAbba [x,y,b,a] 
    | x == y = False
    | x == a && y == b = True
    | otherwise = False
isAbba _ = False

checkPartForAbba :: String -> (Bool, String)
checkPartForAbba [] = (False, [])
checkPartForAbba part 
    | isAbba partial == True = (True, partial)
    | otherwise = checkPartForAbba $ tail part
    where
        partial = take 4 part

sampleIp :: T.Text
sampleIp = "ioxxoj[asdfgh]zxcvbn"

--createIp :: T.Text -> IP 
createIp ipAddress = 
    let
        openSplit = T.splitOn "["
        closeSplit = T.splitOn "]"
        parts = mconcat $ map openSplit (closeSplit ipAddress)
        net = netSequence parts
        hyper = hypernetSequence parts
     in
        IP net hyper

netSequence :: [T.Text] -> [String]
netSequence [t] = [T.unpack t]
netSequence (t:h:ts) = T.unpack t : netSequence ts

hypernetSequence :: [T.Text] -> [String]
hypernetSequence [t] = []
hypernetSequence (t:h:ts) = T.unpack h : hypernetSequence ts        
        
isValidIp :: IP -> Bool
isValidIp (IP net hyper) = 
    let
        anyAbba = any (fst . checkPartForAbba) -- . mconcat
        anyNet = anyAbba net
        anyHyper = anyAbba hyper
    in
        not anyHyper && anyNet