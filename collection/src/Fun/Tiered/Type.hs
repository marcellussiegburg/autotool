-- | tiered (and primitive) recursion.


{-# LANGUAGE TemplateHaskell #-}

module Fun.Tiered.Type 

( Tier (..), Arity (..)
, Fun (..), tier, arity, Exp (..), Mark (..)
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

newtype Tier = T Int deriving ( Eq, Ord ) 
newtype Arity = A Int deriving ( Eq, Ord )

$(derives [makeToDoc] [''Tier, ''Arity ])

nat = do i <- my_integer ; guard (i>=0) ; return $ fromIntegral i
  <?> "nonnegative literal"

instance Reader Tier where
  reader = my_reserved "T" *> (T <$> nat)

instance Reader Arity where
  reader = my_reserved "A" *> (A <$> nat)


-- | first argument is tier, second is arity
data Fun = 
         -- | Grundfunktionen
           Zero Tier Arity
         | Succ0 Tier Arity -- ^ x mapsto 2*x          
         | Succ1 Tier Arity -- ^ x mapsto 2*x+1
         | Proj Tier Arity Int

         -- | so tun, also ob Grundfunktion
         | Builtin Tier Arity RAM.Builtin.Builtin

         -- | Operatoren
         | Sub Tier Arity [ Fun ]
         | PR  Tier Arity [ Fun ]

    deriving (Eq, Ord, Typeable)

tier :: Fun -> Tier
tier f = case f of
  Zero t _ -> t ; Succ0 t _ -> t ; Succ1 t _ -> t
  Proj t _ _ -> t ; Builtin t _ _ -> t
  Sub t _ _ -> t ; PR t _ _ -> t

arity :: Fun -> Arity
arity f = case f of
  Zero _ a -> a ; Succ0 _ a -> a ; Succ1 _ a -> a
  Proj _ a _ -> a ; Builtin _ a _ -> a
  Sub _ a _ -> a ; PR _ a _ -> a

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





