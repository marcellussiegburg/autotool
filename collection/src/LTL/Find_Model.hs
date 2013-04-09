{-# language TemplateHaskell, DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}

module LTL.Find_Model (make_fixed) where

import Autolib.LTL.Type
import Autolib.LTL.Assign
import Autolib.LTL.Eval.ORW
import Autolib.ORW.Type as O

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Inter.Types
import Inter.Quiz

import Data.Typeable
import qualified Data.Map as M
import qualified Data.Set as S

data LTL_Find_Model = LTL_Find_Model 
  deriving (Typeable, Show)

derives [makeReader, makeToDoc] [''LTL_Find_Model]

instance OrderScore LTL_Find_Model where
    scoringOrder _ = None

instance Partial LTL_Find_Model Formula 
         (ORW Assign ) where
    describe _ f = vcat
        [ text "Gesucht ist ein omega-reguläres Modell"
        , text "für die PLTL-Formel"
        , nest 4 $ toDoc f
        ]

    -- FIXME: should only contain variables
    -- that are mentioned in the formula
    initial _ f = 
       read "{p=True,q=False} ({p=False,q=False} {p=False,q=True})^omega"

    partial _ f o = silent $ do
        let vs = S.fromList $ variables f
        inform $ text "die Variablenmege der Formel ist"
               <+> toDoc vs
        forM_ ( O.tails o ) $ \ o -> do
            let a @ (Assign m) = O.head o 
            assert (M.keysSet m == vs) 
                   $ text "stimmt überein mit der Variablenmenge des Zustandes" <+> toDoc a <+> text "?"

    total _ f o = do
        v <- evaluate f o
        assert v $ text "Wert der Formel ist True?"

make_fixed :: Make
make_fixed = direct LTL_Find_Model 
           $ (read "G F p && not F G p" :: Formula )