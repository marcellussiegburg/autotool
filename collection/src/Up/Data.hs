{-# language DeriveDataTypeable #-}

module Up.Data where

import Autolib.Size

import Data.Generics.Schemes

import Data.Data
import Data.Typeable


instance Size Block where 
    size = gsize

newtype Name = Name String
    deriving ( Eq, Ord, Typeable, Data )

data Typ = TUnit | TInt | TBool | TFunc [ Typ ]
    deriving ( Eq, Ord, Typeable, Data )    

data TypedName = TypedName Typ Name
    deriving ( Eq, Ord, Typeable, Data )    

data Statement
     = Declaration Name Exp
     | Statement Exp
     | Missing
     | Halt
    deriving ( Eq, Ord, Typeable, Data )

data Block = Block [ Statement ]
    deriving ( Eq, Ord, Typeable, Data )

data Exp = ConstInteger Integer
         | Ref Name
         | Program [ Name ] Block
         | App Exp [ Exp ]
    deriving ( Eq, Ord, Typeable, Data )

