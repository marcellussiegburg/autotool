{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances #-}
{-# language DeriveGeneric #-}

module Robots.Data where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Data.Typeable
import GHC.Generics

data Robots = Robots deriving ( Typeable )
data Robots_Inverse = Robots_Inverse deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Robots])
$(derives [makeReader, makeToDoc] [''Robots_Inverse])

instance Show Robots where show = render . toDoc
instance Show Robots_Inverse where show = render . toDoc
                         
type Position = ( Integer, Integer )

data Robot = Robot { name :: String
		   , position :: Position
		   , ziel :: Maybe Position
		   }
     deriving ( Eq, Ord, Typeable, Generic )

$(derives [makeReader, makeToDoc] [''Robot])

instance Hashable Robot 
--    where hash r = hash ( name r, position r, ziel r )

data Richtung = N | O | S | W 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

$(derives [makeReader, makeToDoc] [''Richtung])

richtungen :: [ Richtung ]
richtungen = [ minBound .. maxBound ]

type Zug = ( String, Richtung )

instance Size Zug where size _ = 1


