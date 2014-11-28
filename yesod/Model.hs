module Model where

import Data.Int (Int)
import Data.Text (Text)
import Data.Maybe (Maybe)

data StudentListe = StudentEintrag Text Text Text Int Int [Ergebnis]
type Ergebnis = Maybe (Int, Int)

type SchuleId = Int
type SemesterId = Int
type VorlesungId = Int
type GruppeId = Int
type AufgabeId = Int
