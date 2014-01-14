{-# language NoMonomorphismRestriction #-}
{-# language ScopedTypeVariables #-}
{-# language Rank2Types #-}

module Up.Shape 
( match 
) where

import Up.Data
import Up.ToDoc

import Autolib.Reporter
import Autolib.ToDoc

import Data.Generics.Aliases
import Data.Generics.Twins

import Data.Data


import Control.Monad

match :: Block -> Block -> Reporter ()
match = match' 

match' :: GenericQ (GenericQ ( Reporter () ))
match' p q =
    if toConstr q == toConstr Missing
    then whine "soll ersetzt werden:" p q  
    else if toConstr p == toConstr Missing
    then return () 
    else if toConstr p /= toConstr q
    then whine "paßt nicht:" p q 
    else sequence_ $ gzipWithQ match' p q

toDoq =    text "??"
    `mkQ` ( \ (e::Exp) -> toDoc e)
    `extQ` ( \ (e::Block) -> toDoc e)
    `extQ` ( \ (e::Name) -> toDoc e)
    `extQ` ( \ (e::Statement) -> toDoc e)

whine msg p q = reject $ vcat 
    [ text msg
    , text "Muster:" <+> toDoq p
    , text "Eingabe:" <+> toDoq q
    ]
