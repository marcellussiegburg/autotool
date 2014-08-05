{-# language DatatypeContexts #-}
{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleContexts #-}

module Rewriting.Termination where

import Rewriting.Termination.Semiring
import Rewriting.Termination.Interpretation

import Autolib.Symbol
import Autolib.Reader
import Autolib.ToDoc
import Rewriting.TRS
import Autolib.FiniteMap
import Data.Typeable

data Restriction = None 
     -- | And Restriction | Or Restriction | Not Restriction
     | Allow_Lexicographic_Combination
     | Allow_Matrix_Natural
     | Allow_Matrix_Arctic
     | Allow_Matrix_Tropical
     | Allow_Matrix_Fuzzy
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Restriction]

data Symbol c => Problem c = 
     Problem { system :: TRS c c
             , restriction :: Restriction
             }
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Problem]

data Order c 
    = Empty
    | Interpretation c
    | Lexicographcic [ Order c ]
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Order]

