{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Data where

import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

data Prop 
    = And [ Prop ]
    | Or [ Prop ]
    | Not Prop
    | Prop1 Prop1 Exp
    | Prop2 Prop2 Exp Exp
    | PropParens Prop
    deriving Typeable

data Prop1 
    = Null | Total
    | Reflexive | Transitive | Symmetric | Antisymmetric 
    | SN  -- ^ Strongly_Normalizing 
    | WN -- ^ Weakly_Normalizing 
    | CR -- ^ Confluent 
    | WCR -- ^ Locally_Confluent 
    | UN -- ^  Unique_Normalforms 
    | UNC -- ^ Unique_Normalforms_wrt_Conversion 
        deriving (Typeable, Show, Enum, Bounded )

data Prop2 
    = Equals | Subsetof | Disjoint
        deriving (Typeable, Show, Enum, Bounded )

data Exp = Ref Identifier
     | Op1 Op1 Exp
     | Op2 Op2 Exp Exp
     | ExpParens Exp
    deriving Typeable

data Op1 = Inverse 
     | Complement
    | Transitive_Closure 
    | Transitive_Reflexive_Closure
        deriving (Typeable, Show)

data Op2 = Union | Intersection | Difference | Product
    deriving (Typeable, Show )



