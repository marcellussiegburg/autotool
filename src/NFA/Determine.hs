module NFA.Determine where

-- vom NFA zum DFA

-- $Id$

import NFA.Type
import NFA.Eq
import qualified NFA.Example
import qualified NFA.Check
import NFA.Restrict
import NFA.Some
import NFA.Minimize
import NFA.Normalize

import Inter.Types
import Util.Datei
import Util.Size
import Util.Cache
import Util.Seed
import Util.Zufall
import Sets
import ToDoc
import Reporter

import qualified Challenger as C


data Determine = Determine deriving ( Eq, Ord, Show, Read )

data DI = DI { nea :: NFA Char Int
	     , alphabet :: Set Char
	     }
    deriving ( Show )


instance C.Partial  Determine DI ( NFA Char Int )
  where
    initial p i   = NFA.Example.example

    partial p i b = do
        restrict_states b
	restrict_alpha ( alphabet i ) b
        NFA.Check.deterministisch b

    total   p i b = do
	f <- equ ( informed ( text "Sprache des gegebenen Automaten") ( nea i ))
		 ( informed ( text "Sprache Ihres Automaten" )        b )

	assert f $ text "Stimmen die Sprachen �berein?"
        return () 

data Conf = Conf
	     { alpha :: Set Char
	     , nea_size :: Int
	     , min_dea_size :: Int
	     , max_dea_size :: Int
	     }
    deriving ( Eq, Ord, Read, Show )

throw :: Conf -> IO ( NFA Char Int, NFA Char Int )
throw conf = repeat_until
    ( do a <- nontrivial ( alpha conf ) ( nea_size conf )
         let d = normalize $ minimize $ a
         return ( a, d )
    ) ( \ ( a, d ) -> min_dea_size conf <= size d 
	           && size d <= max_dea_size conf
    )

determine :: String -- aufgabe (major)
	 -> String -- aufgabe (minor)
	 -> Conf
	 -> Var  Determine DI ( NFA Char Int )
determine auf ver conf = 
    Var { problem = Determine
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do 
	      return matrikel
	, gen = \ key -> do
	      seed $ read key
	      ( i, b ) <- cache (  Datei { pfad = [ "autotool", "cache"
                                           , auf, ver
                                           ]
                                  , name = key ++ ".cache"
                                  ,relativzahl = error "NFA.Determine.relativzahl"
                                  }
                         ) ( throw conf )
              return $ do
	          inform $ vcat
	             [ text "Gegeben ist der Automat"
		     , nest 4 $ toDoc i
		     , text "Finden Sie einen dazu �quivalenten deterministischen Automaten!"
		     ]
	          return $ DI { nea = i
			      , alphabet = alpha conf
			      }
	}


