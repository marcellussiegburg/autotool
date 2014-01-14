{-# language NoMonomorphismRestriction #-}
{-# language ScopedTypeVariables #-}
{-# language Rank2Types #-}

module Up.Shape 
( match 
) where

import Up.Data
import Up.ToDoc
import Up.Reader

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
         if eqTop q Missing
    then whine "soll ersetzt werden:" p q  
    else if eqTop Missing p 
    then return () 
    else if eqTop p q 
    then sequence_ $ gzipWithQ match' p q
    else whine "paßt nicht:" p q 

eqTop p q = toType p == toType q 
         && toConstr p == toConstr q

-- FIXME: there is no instance Eq DataType
-- and the instance Eq DataRep is not usable
toType p = dataTypeName $ dataTypeOf p


toDoq :: GenericQ Doc
toDoq =    ( \ e -> text (show (toConstr e)))
    `extQ`  ( \ (e::Exp) -> toDoc e)
    `extQ` ( \ (e::[Exp]) -> toDoc e)
    `extQ` ( \ (e::Block) -> toDoc e)
    `extQ` ( \ (e::String) -> toDoc $ Name e)
    `extQ` ( \ (e::Name) -> toDoc e)
    `extQ` ( \ (e::[Name]) -> toDoc e)
    `extQ` ( \ (e::Statement) -> toDoc e)
    `extQ` ( \ (e::[Statement]) -> toDoc e)

whine :: String -> GenericQ (GenericQ (Reporter ()))
whine msg p q = reject $ vcat 
    [ text msg
    , text "Muster :" <+> toDoq p
    , text "Eingabe:" <+> toDoq q
    ]
