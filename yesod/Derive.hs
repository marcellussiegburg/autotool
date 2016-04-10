{-# OPTIONS_GHC -fno-warn-orphans #-}
module Derive where

import Database.Persist.TH (derivePersistField)
import Autolib.Multilingual (Language (..))
import Control.Types (HiLo(..), Status(..))
import Operate.Crypt (Crypt (Crypt))

derivePersistField "Crypt"
derivePersistField "Language"
derivePersistField "HiLo"
derivePersistField "Status"
