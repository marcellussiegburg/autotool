-- | tiered (and primitive) recursion.


{-# LANGUAGE TemplateHaskell #-}

module Fun.Poly.Type 

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

-- | first argument is tier, second is arity
data Fun = 
         -- | Grundfunktionen
           Zero Int Int
         | Succ0 Int Int -- ^ x mapsto 2*x          
         | Succ1 Int Int -- ^ x mapsto 2*x+1
         | Proj Int Int Int

         -- | so tun, also ob Grundfunktion
         | Builtin Int Int RAM.Builtin.Builtin

         -- | Operatoren
         | Sub Int Int [ Fun ]
         | PR  Int Int [ Fun ]

    deriving (Eq, Ord, Typeable)

instance Size Fun where
    size ( Sub _ _ fs ) = succ $ sum $ map size fs 
    size ( PR  _ _ fs ) = succ $ sum $ map size fs 
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





