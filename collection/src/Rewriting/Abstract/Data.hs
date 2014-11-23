{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Data where

import Autolib.TES.Identifier
import Autolib.ToDoc hiding ( Full )
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

instance Size Prop where
    size p = case p of
        And ps -> succ $ sum $ map size ps
        Or  ps -> succ $ sum $ map size ps
        Not p -> succ $ size p
        PropParens p -> size p
        Prop1 _ p -> succ $ sum $ map size [p]
        Prop2 _ p q -> succ $ sum $ map size [p,q]

data Prop1 
    = Null | Full
    | Reflexive | Irreflexive
    | Transitive 
    | Symmetric | Antisymmetric | Asymmetric
    | Total
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

instance Size Exp where
    size x = case x of
        ExpParens x -> size x
        Ref _ -> 1
        Op1 _ x -> succ $ size x
        Op2 _ x y -> succ $ sum $ map size [x,y]

data Op1 = Inverse 
     | Complement
    | Transitive_Closure 
    | Transitive_Reflexive_Closure
        deriving (Typeable, Show)

data Op2 = Union | Intersection | Difference | Product
    deriving (Typeable, Show )

derives [makeReader,makeToDoc] [''Prop1, ''Prop2, ''Op1, ''Op2]


