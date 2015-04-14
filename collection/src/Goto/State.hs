{-# language DeriveDataTypeable #-}

module Goto.State where

import Goto.Type
import Goto.Memory
import Machine.History

import Autolib.ToDoc
import Data.Typeable


data State =
     State { schritt :: Int
	   , memory :: Memory
	   , pc   :: Int -- ^ number of next statement
           , program :: Program 
	   , past :: [State] -- ^ previous states
	   }
     deriving ( Eq, Ord, Typeable )

instance ToDoc State where
    toDoc s = named_dutch_record (text "State")
	    [ text "schritt" <+> equals <+> toDoc ( schritt s )
	    , text "memory" <+> equals <+> toDoc ( memory s )
	    , text "pc" <+> equals <+> toDoc (pc s)
	    ]

instance History State where
    history = past

