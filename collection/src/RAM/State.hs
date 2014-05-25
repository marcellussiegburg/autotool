module RAM.State where

import RAM.Type
import RAM.Memory
import Machine.History

import Autolib.ToDoc
import Data.Typeable


data State =
     State { schritt :: Int
	   , memory :: Memory
	   , todo   :: Program -- noch auszuführen
	   , past :: [State] -- vorige zustände
	   }
     deriving ( Eq, Ord, Typeable )

instance ToDoc State where
    toDoc s = named_dutch_record (text "State")
	    [ text "schritt" <+.> equals <+.> toDoc ( schritt s )
	    , text "memory" <+.> equals <+.> toDoc ( memory s )
	    , text "todo" <+.> equals 
	      <+.> dutch_list ( take 1 ( map toDoc $ todo s )
                              ++ [ text "..." | not (null $ drop 1 $ todo s) ] )
	    ]

instance History State where
    history = past

