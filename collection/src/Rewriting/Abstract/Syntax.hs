{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Syntax where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

data Boolean p = And [ Boolean p ]
               | Or [ Boolean p ]
               | Not ( Boolean p )
               | Atomic p
    deriving Typeable

data Prop 
    = Prop1 Prop1 Exp
    | Prop2 Prop2 Exp Exp
    deriving Typeable

data Prop1 
    = Reflexive | Transitive | Symmetric | Antisymmetric 
    | Strongly_Normalizing | SN
    | Weakly_Normalizing | WN
    | Confluent | CR
    | Locally_Confluent | WCR
    | Unique_Normalforms | UN
    | Unique_Normalforms_wrt_Conversion | UNC
        deriving Typeable

data Prop2 
    = Equals | Subseteq
    deriving Typeable

data Exp = Identifier 
     | Op1 Op1 Exp
     | Op2 Op2 Exp Exp
    deriving Typeable

data Op1 = Inverse
    | Transitive_Closure 
    | Transitive_Reflexive_Closure
        deriving Typeable

data Op2 = Union | Intersection | Difference | Product
    deriving Typeable

derives [makeReader, makeToDoc] 
    [''Boolean,''Prop, ''Prop1, ''Prop2, ''Exp, ''Op1, ''Op2]

