{-# language DatatypeContexts #-}
{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module Rewriting.Termination where

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

data Problem v c = 
     Problem { system :: TRS v c
             , restriction :: Restriction
             }
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Problem]

data Multilinear d = 
     Multilinear { absolute :: Vector d, linear :: [Matrix d] }
    deriving ( Eq, Ord, Typeable )

data Order c 
    = Empty
    | Matrix_Natural (FiniteMap c (Multilinear Natural))
    | Matrix_Arctic (FiniteMap c (Multilinear Arctic))
    | Matrix_Tropical (FiniteMap c (Multilinear Tropical))
    | Matrix_Fuzzy (FiniteMap c (Multilinear Fuzzy))
    | Lexicographcic [ Order c ]
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Problem]

