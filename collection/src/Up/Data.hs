{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Up.Data where

import Autolib.Size

import Data.Generics.Schemes

import Data.Data
import Data.Typeable

import Test.QuickCheck as Q
import Data.DeriveTH
import Control.Applicative

import Test.SmallCheck.Series as S
import Data.Generics

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


-- * not nice:

instance Monad m => Serial m Name where
    series = ( \ ( S.Positive i ) -> Name $ "a" ++ show (i::Int) ) <$> series

instance Monad m => Serial m Typ where
    series = cons0 TUnit \/ cons0 TInt \/ cons0 TBool \/ cons1 TFunc 

instance Monad m => Serial m TypedName where
    series = cons2 TypedName

instance Monad m => Serial m Statement where
    series = cons2 Declaration \/ cons1 Statement \/ cons0 Missing \/ cons0 Halt

instance Monad m => Serial m Exp where
    series = cons1 ConstInteger \/ cons1 Ref \/ cons2 Program \/ cons2 App

instance Monad m => Serial m Block where
    series = cons1 Block

-- * also not nice:
    
instance Arbitrary Name where
  arbitrary =
    ( \ ( Q.Positive i ) -> Name $ "a" ++ show (i::Int) )
    <$> arbitrary

derive makeArbitrary ''Typ
derive makeArbitrary ''TypedName
derive makeArbitrary ''Statement
derive makeArbitrary ''Block
derive makeArbitrary ''Exp
