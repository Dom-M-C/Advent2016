module Janonymizer where 


import qualified Data.ByteString as BS
import qualified Data.Text as T
--import Data.Unique.Id
import GHC.IOBase 

type Id = Int


type CompressedJson = BS.ByteString
type JsonString = T.Text

data EventMetadata = EventMetadata
    {   eventId :: Integer
    ,   compressedJson :: CompressedJson
    }

data IntegrationEvent = UnclassifiedEvent { eventMetadata :: EventMetadata }
    | UnderwritingDecisionMadeEvent
    {   eventMetadata :: EventMetadata
    ,   journeyId :: Id
    ,   subjectId :: Id
    }


eventsToAnonymize :: IntegrationEvent -> [IntegrationEvent]
eventsToAnonymize (UnderwritingDecisionMadeEvent meta) = undefined
eventsToAnonymize _ = []


janonymizer :: [IntegrationEvent] -> [IntegrationEvent]
janonymizer = map anonymizeEvent

anonymize :: IntegrationEvent -> IntegrationEvent
anonymize = applyStrategies . decompress . compressedJson

decompress :: CompressedJson -> JsonString
decompress = undefined

applyStrategies = undefined

