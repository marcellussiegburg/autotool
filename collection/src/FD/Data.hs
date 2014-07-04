{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}

module FD.Data where

import Autolib.Reader
import Autolib.ToDoc

import qualified Data.Set as S
import Data.Typeable
import Control.Applicative ((<$>),(<*>))

f :: Formula
f = [ Atom (Rel "<") [ Var "x", Var "y" ]
    , Atom (Rel "<") [ Var "y", Var "z" ]
    , Atom (Rel "<") [ Var "z", Var "y" ]
    ]

newtype Var = Var { unVar :: String }
    deriving (Eq, Ord, Typeable )

instance ToDoc Var where toDoc = text . unVar
instance Show Var where show = render . toDoc
instance Reader Var where reader = Var <$> my_identifier

newtype Rel = Rel { unRel :: String }
    deriving (Eq, Ord, Typeable )

instance ToDoc Rel where toDoc = text . unRel
instance Show Rel where show = render . toDoc
instance Reader Rel where reader = Rel <$> my_identifier

data Atom = Atom { rel :: Rel, args :: [ Var ] }
    deriving (Eq, Ord, Typeable )

instance ToDoc Atom where 
    toDoc (Atom rel args) = toDoc rel <+> dutch_tuple (map toDoc args)
instance Show Atom where show = render . toDoc
instance Reader Atom where
    reader = Atom <$> reader <*> my_parens ( my_commaSep reader )
             
type Formula = [ Atom ]

variables :: Formula -> S.Set Var
variables = S.fromList . concat . map args 
