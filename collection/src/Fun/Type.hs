{-# LANGUAGE TemplateHaskell #-}

module Fun.Type 

( Fun (..), Exp (..), Mark (..)
, RAM.Builtin.Builtin (..)
, Property (..)
)

where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Set
import Autolib.Xml

import qualified RAM.Builtin

import Data.Typeable

data Property = Builtins [ RAM.Builtin.Builtin ]
    deriving ( Eq, Ord, Typeable )	      

$(derives [makeReader, makeToDoc] [''Property])

instance Show Property where show = render . toDoc

data Fun = 
	 -- | Grundfunktionen
	   Zero Int
	 | Succ Int
	 | Decr Int -- ^ naja
	 | Proj Int Int

         | Binary_Succ0 Int
         | Binary_Succ1 Int

	 -- | so tun, also ob Grundfunktion
	 | Builtin Int RAM.Builtin.Builtin

	 -- | Operatoren
	 | Sub Int [ Fun ]
	 | PR  Int [ Fun ]
	 | Min Int [ Fun ]

	 | Binary_PR  Int [ Fun ]

    deriving (Eq, Ord, Typeable)

instance Size Fun where
    size ( Sub i fs ) = succ $ sum $ map size fs 
    size ( PR  i fs ) = succ $ sum $ map size fs 
    size ( Min i fs ) = succ $ sum $ map size fs 
    size _ = 1

data Exp
        = Zahl Integer
	-- | non-strikt
        | App Fun [ Exp ]
	-- | nur auf dem Stack benutzt, für Builtins (die sind strikt)
	-- wende builtin auf die obersten stack-elemente an
	| Builtin_ Int RAM.Builtin.Builtin
	-- | benutzt für PRs, deren letztes arg schon auf stack steht
	| App_ Fun [ Exp ]
	-- | top of stack in cache eintragen
	| M Mark
    deriving ( Eq, Ord, Typeable )


-- | wollen wir nicht ausgeben
data Mark = Mark Exp deriving (Eq, Ord, Typeable)

instance ToDoc  Mark where toDoc m = text "{..}"
instance Reader Mark -- ohne implementierung

$(derives [makeReader, makeToDoc] [''Fun])

$(derives [makeReader, makeToDoc] [''Exp])





