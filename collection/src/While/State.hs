module While.State where

import While.Type
import While.Memory
import Machine.History

import Autolib.ToDoc
import Data.Typeable


data State =
     State { schritt :: Int
	   , memory :: Memory
	   , todo :: [Program ]
	   , past :: [State] -- ^ vorige zustände
	   }
     deriving ( Eq, Ord, Typeable )

instance ToDoc State where
    toDoc s = named_dutch_record (text "State")
	    [ text "schritt" <+.> equals <+.> toDoc ( schritt s )
	    , text "memory" <+.> equals <+.> toDoc ( memory s )
	    , text "todo" <+.> equals 
	      <+.> dutch_list (map (shorten 2) $ todo s)
	    ]

instance History State where
    history = past

