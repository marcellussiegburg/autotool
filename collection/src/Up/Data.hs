{-# language DisambiguateRecordFields #-}
{-# language DeriveDataTypeable #-}

module Up.Data where

import Data.Typeable

newtype Name = Name String
    deriving ( Eq, Ord, Typeable )

data Typ = TUnit | TInt | TBool | TFunc [ Typ ]
    deriving ( Eq, Ord, Typeable )    

data TypedName = TypedName Typ Name
    deriving ( Eq, Ord, Typeable )    

data Statement
     = Declaration TypedName Exp
     | Statement Exp
    deriving ( Eq, Ord, Typeable )

data Block = Block  [Statement]
    deriving ( Eq, Ord, Typeable )

data Exp = ConstInteger Integer
         | Ref Name
         | Program { parameters :: [ TypedName ]
             , body :: Block
             }
         | App Exp [ Exp ]
    deriving ( Eq, Ord, Typeable )

