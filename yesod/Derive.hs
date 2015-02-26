{-# OPTIONS_GHC -fno-warn-orphans #-}
module Derive where

import Database.Persist.TH (derivePersistField)
import Autolib.Multilingual (Language (..))

derivePersistField "Language"
