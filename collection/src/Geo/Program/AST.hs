{-# language DeriveDataTypeable #-}

module Geo.Program.AST where

import Data.Typeable

data Exp v c
     = Ref v
     | Apply (Exp v c) [ Exp v c ]
     | Block [ Decl v c ] ( Exp v c )
    deriving Typeable

data Typed v = Typed Type v
    deriving Typeable
             
data Decl v c = Decl (Typed v) (Maybe [ Typed v ]) (Exp v c)
    deriving Typeable

data Type = Point | Line | Circle
    deriving Typeable
