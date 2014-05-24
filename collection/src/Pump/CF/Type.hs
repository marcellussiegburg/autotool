{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Pump.CF.Type where

import Autolib.Size
import Autolib.Hash
import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import GHC.Generics

data Zerlegung = Zerlegung
	       { u :: String, v :: String
	       , x :: String, y :: String, z :: String }
     deriving (Eq, Ord, Typeable, Generic)

$(derives [makeReader, makeToDoc] [''Zerlegung])

instance Show Zerlegung where show = render . toDoc
                              
instance Hash Zerlegung 





