{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

module Graph.Color where

import Data.Typeable
import Autolib.ToDoc
import Data.Autolib.Transport
import Autolib.Reader
import Autolib.Hash
import GHC.Generics

data Color = A | B | C | D | E | F | G | H | I | J | K | L | M 
           | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
     deriving ( Eq, Ord, Typeable, Enum, Bounded, Generic )

instance Hash Color 

$(derives [makeReader, makeToDoc, makeToTransport] [''Color])

instance Show Color where show = render . toDoc


