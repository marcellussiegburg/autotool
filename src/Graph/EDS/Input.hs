{-# LANGUAGE TemplateHaskell #-}

-- | perfect vertex cover (used for construction of code)

module Graph.PVC.Input where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

import SAT.Types

data Input = Input
	    { formel          :: Formel
	    , anzeige_groesse :: Int	
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Input])

i0 :: Input
i0 = Input { formel = read "(x || y || z) && (! x || y || !z )"
	   , anzeige_groesse = 6 
	   }


