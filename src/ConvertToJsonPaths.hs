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


getJsonName :: T.Text -> T.Text
getJsonName line
    | snd colonBreak == "" = ""
    | otherwise = T.strip . T.replace "\"" "" . fst $ colonBreak
    where 
        colonBreak = T.breakOn ":" line


dropJsonLevel = T.drop 1 . T.reverse . snd . T.breakOn "." . T.reverse

applicant_DataStoredEvent :: T.Text
applicant_DataStoredEvent = 
    "{  \
    \ \n  \"JourneyId\": \"a68ed7fb-b8c3-4b80-bd4c-c9d2971b1003\", \
    \ \n  \"SubjectId\": \"1cded55e-02e3-418d-b61c-f39ba8e6dc77\", \
    \ \n  \"Type\": \"Applicant\", \
    \ \n  \"Status\": \"DataRetrieved\", \
    \ \n  \"Data\": { \
    \ \n    \"firstName\": \"Dominic\", <-- PII \
    \ \n    \"lastName\": \"Schnitzel\", <-- PII \
    \ \n    \"dateOfBirth\": \"1990-01-01T00:00:00Z\",<-- PII \
    \ \n    \"emailAddress\": \"dom@schnitzel.com\",<-- PII \
    \ \n    \"mobilePhoneNumber\": \"\",<-- PII \
    \ \n    \"homePhoneNumber\": \"\",<-- PII \
    \ \n    \"dependants\": null, \
    \ \n    \"housingContribution\": null, \
    \ \n    \"currentAddress\": { \
    \ \n      \"residentialStatus\": \"Homeowner\", \
    \ \n      \"postcode\": \"NG15FW\", <-- PII \
    \ \n      \"movedInAt\": \"2014-11-11T00:00:00Z\" \
    \ \n    }, \
    \ \n    \"previousAddress\": null <-- Possible PII \
    \ \n  }, \
    \ \n  \"TimeStamp\": \"2019-11-11T12:55:45.5578816Z\" \
    \ \n}"
