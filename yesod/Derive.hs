{-# OPTIONS_GHC -fno-warn-orphans #-}
module Derive where

import Database.Persist.TH (derivePersistField)
import Autolib.Multilingual (Language (..))
import Control.Types (HiLo(..), Status(..))

derivePersistField "Language"
derivePersistField "HiLo"
derivePersistField "Status"
