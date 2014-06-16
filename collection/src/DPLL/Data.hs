{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}

module DPLL.Data where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

newtype Variable = Variable { unVariable :: Int }
    deriving (Eq, Ord, Num )

instance Reader Variable where 
    reader = do
        v <- my_integer
        if v > 0 then return $ Variable $ fromIntegral v
                 else unexpected "variable name must be positive"
instance ToDoc Variable where
    toDoc (Variable v) = toDoc v
instance Show Variable where show = render . toDoc

newtype Literal =  Literal { unLiteral :: Int }
    deriving (Eq, Ord, Num )

instance Reader Literal where 
    reader = do
        l <- my_integer
        if l /= 0 then return $ Literal $ fromIntegral l
                 else unexpected "literal cannot be 0 (since 0 is not a variable)"

instance ToDoc Literal where
    toDoc (Literal l) = toDoc l
instance Show Literal where show = render . toDoc


type Clause = [ Literal ]
type CNF = [ Clause ]

variable :: Literal -> Variable
variable  = Variable . abs . unLiteral

positive :: Literal -> Bool
positive = ( > 0 ) . unLiteral

negative :: Literal -> Bool
negative = ( < 0 ) . unLiteral


mkLiteral :: Variable -> Bool -> Literal
mkLiteral (Variable v) b = Literal $ ( if b then id else negate ) v

cnf0 :: CNF
cnf0 = [[1,2,3],[-1,-2],[2,-3],[1,-3],[-2,3]]
