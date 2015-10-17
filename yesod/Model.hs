{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Model where

import Prelude (Integer, div, fromInteger, toInteger, (^), (*), (.))
import Data.Bool (Bool)
import Data.Either (Either (Right))
import Data.Fixed (Fixed (MkFixed))
import Data.Function (id, ($))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Ord ((<), (<=))
import Data.Text (Text, unpack, pack)
import Data.Text.Read (decimal, signed)
import Data.Time (Day, TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian, timeOfDayToTime, timeToTimeOfDay, todHour, todMin, todSec, toGregorian, utctDay, utctDayTime)
import Data.Tree as Import (Tree (Node), rootLabel, subForest)
import Database.Persist.Quasi
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Yesod (mkMigrate, mkPersist, share, sqlSettings, persistFileWith)
import Yesod.Core.Dispatch (PathPiece, fromPathPiece, toPathPiece)

import Derive()

import Autolib.Multilingual (Language (..))
import qualified Control.Schule.Typ as Schule
import qualified Control.Vorlesung.Typ as Vorlesung
import Control.Types
import Control.Time.Typ (Time (Time))
import Types.TaskTree (TaskTree (Category, Task))

type AufgabeId = Int
type AufgabeKonfiguration = Text
type AufgabeTyp = Text
type GruppeId = Int
type VorlageName = Text
type ServerUrl = Text
type StudentId = Int

keyToInt = fromInteger . toInteger . fromSqlKey

intToKey = toSqlKey . fromInteger . toInteger

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

entityToSchule key entity = Schule.Schule {
    Schule.unr = UNr $ keyToInt key,
    Schule.name = Name $ unpack $ schuleName entity,
    Schule.preferred_language = schulePreferredLanguage entity,
    Schule.mail_suffix = Name $ unpack $ maybe "" id $ schuleMailSuffix entity,
    Schule.use_shibboleth = schuleUseShibboleth entity
  }

entityToVorlesung key entity = Vorlesung.Vorlesung {
    Vorlesung.vnr = VNr $ keyToInt key,
    Vorlesung.name = Name $ unpack $ vorlesungName entity,
    Vorlesung.einschreibVon = utcTimeToTime $ vorlesungVon entity,
    Vorlesung.einschreibBis = utcTimeToTime $ vorlesungBis entity,
    Vorlesung.unr = UNr $ keyToInt $ vorlesungSchuleId entity,
    Vorlesung.enr = ENr $ keyToInt $ vorlesungSemesterId entity,
    Vorlesung.motd = Name $ unpack $ maybe "" id $ vorlesungNachricht entity,
    Vorlesung.einschreib = Current
  }

instance PathPiece UNr where
    fromPathPiece u =
        case signed decimal u of
          Right (u', "") -> Just $ UNr u'
          _ -> Nothing
    toPathPiece u = pack $ toString u

instance PathPiece MNr where
    fromPathPiece u = Just $ MNr $ unpack u
    toPathPiece u = pack $ toString u

timeToUTCTime :: Time -> UTCTime
timeToUTCTime (Time y m d h min s) = UTCTime (fromGregorian (toInteger y) m d) $ timeOfDayToTime $ TimeOfDay h min $ MkFixed $ toInteger s * 10 ^ (12 :: Integer)

utcTimeToTime :: UTCTime -> Time
utcTimeToTime utcTime =
  let day  = utctDay utcTime
      time = timeToTimeOfDay $ utctDayTime utcTime
  in dayTimeToTime day time

dayTimeToTime :: Day -> TimeOfDay -> Time
dayTimeToTime day time =
  let (y, m, d) = toGregorian $ day
      MkFixed picos = todSec time
  in Time (fromInteger y) m d (todHour time) (todMin time) (fromInteger $ picos `div` 10 ^ (12 :: Integer))

taskTreeToTextTree :: TaskTree -> Tree Text
taskTreeToTextTree taskTree = case taskTree of
  Task task -> Node (pack task) []
  Category name taskTrees -> Node {
      rootLabel = pack name,
      subForest = fmap taskTreeToTextTree $ taskTrees
    }

zeitStatus :: UTCTime -> UTCTime -> UTCTime -> TimeStatus
zeitStatus von bis time =
  if time < von
  then Early
  else if time <= bis
       then Current
       else Late
