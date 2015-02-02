module Model where

import Prelude (Integer, div, fromInteger, toInteger, (^), (*))
import Data.Either (Either (Right))
import Data.Fixed (Fixed (MkFixed))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text, unpack, pack)
import Data.Text.Read (decimal, signed)
import Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian, timeOfDayToTime, timeToTimeOfDay, todHour, todMin, todSec, toGregorian, utctDay, utctDayTime)
import Yesod.Core.Dispatch (PathPiece, fromPathPiece, toPathPiece)

import Control.Types
import Control.Time.Typ (Time (Time))

type AufgabeId = Int
type AufgabeKonfiguration = Text
type AufgabeTyp = Text
type GruppeId = Int
type VorlageName = Text
type SchuleId = Int
type SemesterId = Int
type ServerUrl = Text
type StudentId = Int
type VorlesungId = Int

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
  let (y, m, d) = toGregorian $ utctDay utcTime
      time = timeToTimeOfDay $ utctDayTime utcTime
      MkFixed picos = todSec time
  in Time (fromInteger y) m d (todHour time) (todMin time) (fromInteger $ picos `div` 10 ^ (12 :: Integer))
