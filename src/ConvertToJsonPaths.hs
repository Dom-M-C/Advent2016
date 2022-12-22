{-# LANGUAGE OverloadedStrings #-}

module ConvertToJsonPaths where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T


piiJson :: IO JsonPaths
piiJson = do
    let json = TIO.readFile "c:\\tmp\\pii.json"
    jsonLines <- tokenLines <$> json
    return $ parsePaths jsonLines mempty

data JsonPaths = Jp 
    {   currentPath :: T.Text
    ,   piiPaths :: [T.Text]
    ,   isInArrayBlock :: Bool
    }

instance Show JsonPaths where
    show (Jp current [] _) = show ("Final path: " <> current)
    show (Jp current (p:aths) _) = show p <> "\n" <> show (Jp current aths False)

instance Semigroup JsonPaths where
    (<>) (Jp _ ps1 _) (Jp _ ps2 _) = Jp "$" (ps1 <> ps2) False

instance Monoid JsonPaths where
    mempty = Jp "$" [] False

type JsonLine = T.Text

lineContainsToken :: JsonLine -> Bool
lineContainsToken line = any (\x -> T.isInfixOf x line) lineToken
    where lineToken = ["<--", "{", "}", "[", "]", "<~~"]

tokenLines :: T.Text -> [JsonLine]
tokenLines = filter lineContainsToken . T.lines

parsePaths :: [JsonLine] -> JsonPaths -> JsonPaths
parsePaths [] jsonPaths = jsonPaths
parsePaths (line:xs) jsonPaths = parsePaths xs $ processLineToPaths line jsonPaths

processLineToPaths :: JsonLine -> JsonPaths -> JsonPaths
processLineToPaths line jp@(Jp current paths inArray)
    | T.isInfixOf "<~~" line = Jp "$" paths False
    | T.isInfixOf "<--" line = Jp current (newPath : paths) inArray
    | T.isInfixOf "[" line = Jp (newPath <> "[*]") paths True
    | T.isInfixOf "]" line = Jp current paths False
    | T.isInfixOf "{" line = Jp newPath paths inArray
    | T.isInfixOf "}" line = Jp (dropJsonLevel current) paths inArray
    where
        jsonName = getJsonName line
        newPath = if jsonName == "" 
            then current
            else T.concat [current, ".", jsonName]


getJsonName :: JsonLine -> JsonLine
getJsonName line
    | snd colonBreak == "" = ""
    | otherwise = T.strip . T.replace "\"" "" . fst $ colonBreak
    where 
        colonBreak = T.breakOn ":" line

dropJsonLevel :: T.Text -> T.Text
dropJsonLevel t
    | extractLowerPath ".." t /= "" = (T.drop 2 . T.reverse . extractLowerPath "..") t
    | otherwise = (T.drop 1 . T.reverse . extractLowerPath ".") t
    where extractLowerPath delimit = (snd . T.breakOn delimit . T.reverse) 
