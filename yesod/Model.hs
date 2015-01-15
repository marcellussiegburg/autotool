module Model where

import Data.Either (Either (Right))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text, unpack, pack)
import Data.Text.Read (decimal, signed)
import Yesod.Core.Dispatch (PathPiece, fromPathPiece, toPathPiece)

import Control.Types

type AufgabeId = Int
type AufgabeKonfiguration = Text
type AufgabeTyp = Text
type GruppeId = Int
type VorlageName = Text
type SchuleId = Int
type SemesterId = Int
type ServerUrl = Text
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
