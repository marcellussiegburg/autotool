{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE DatatypeContexts #-}

module Rewriting.Derive.Instance where


import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size
import Autolib.Multilingual
import Autolib.Reader
import Control.Monad (when)

import Data.Typeable

-- | TODO: boolean combination
data Object_Restriction object = Fixed object | Sized Ordering Int
    deriving (Eq, Typeable)

instance ToDoc object => Nice (Object_Restriction object) where
    nice or = case or of
        Fixed o -> toDoc o
        Sized ord s -> hsep 
            [ multitext [(DE, "ein Objekt der Größe"), (UK, "an object of size")]
            , case ord of LT -> text "<" ; EQ -> empty ; GT -> text ">"
            , toDoc s
            ]

check_object_restriction msg or ob = case or of
    Fixed o -> when (ob /= o) $ reject $ vcat
        [ msg <+> multitext [(DE,"ist nicht"),(UK,"is not")] <+> toDoc o ]
    Sized ord s -> when (compare (size ob) s /= ord) $ reject $ vcat
        [ msg <+> multitext [(DE,"ist nicht"),(UK,"is not")] <+> nice or ]

derives [makeReader, makeToDoc] [''Object_Restriction]

data Derivation_Restriction = Length Ordering Int
    deriving (Eq, Typeable)

instance Nice Derivation_Restriction where
    nice dr = case dr of
        Length ord l -> hsep
            [ multitext [(DE, "mit Länge"), (UK, "of length")]
            , case ord of LT -> text "<" ; EQ -> empty ; GT -> text ">"
            , toDoc l
            ]

check_derivation_restriction msg dr l = case dr of
    Length ord ell -> when (compare l ell /= ord) $ reject $ hsep
        [ msg <+> multitext [(DE,"ist nicht"),(UK,"is not")] <+> nice dr ]

derives [makeReader] [''Ordering]
derives [makeReader, makeToDoc] [''Derivation_Restriction ]

data (  Reader system, ToDoc system
     , Reader object, ToDoc object
     ) 
    => Instance system object = Instance
        { system :: system
        , derivation_restriction :: Derivation_Restriction 
        , from   :: Object_Restriction object
        , to     :: Object_Restriction object
        }
    deriving ( Typeable 
	     )

derives [makeReader, makeToDoc] [''Instance]


