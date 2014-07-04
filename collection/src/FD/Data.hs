{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}

module FD.Data where

f :: Formula
f = [ Atom (Rel "<") [ Var "x", Var "y" ]
    , Atom (Rel "<") [ Var "y", Var "z" ]
    , Atom (Rel "<") [ Var "z", Var "y" ]
    ]

newtype Var = Var { unVar :: String }
    deriving (Eq, Ord, Show )

newtype Rel = Rel { unRel :: String }
    deriving (Eq, Ord, Show )

data Atom = Atom Rel [ Var ]
    deriving (Eq, Ord, Show )
             
type Formula = [ Atom ]

