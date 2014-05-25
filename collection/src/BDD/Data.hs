{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}

module BDD.Data where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Control.Applicative ((<$>))
import Data.Typeable

-- | Addresses must be [0, 1 .. ], root node is 0
data BDD v =  BDD [ (Address, Node v) ]
    deriving Typeable

bdd0 :: [v] -> BDD v
bdd0 (v1: v2:rest) = 
    BDD [ ( 0, Branch 1 v1 2), (1,Branch 3 v2  2), (2,Leaf True), (3,Leaf False)]

data Node v = Leaf Bool | Branch Address v Address
    deriving ( Eq, Ord )

instance Size (BDD v) where size (BDD table) = length table

newtype Address = Address Int deriving ( Eq, Ord, Enum, Num )

newtype Variable = Variable Int deriving ( Eq, Ord, Enum )

derives [makeReader, makeToDoc] [''Node, ''BDD]

instance ToDoc Address where
    toDoc (Address a) = toDoc a
instance Reader Address where
    reader = Address <$> reader

instance ToDoc Variable where
    toDoc (Variable v) = text $ "v" ++ show v
instance Reader Variable where
    reader = do Autolib.Reader.char 'v' ; Variable <$> reader

