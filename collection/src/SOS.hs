-- | sorting without memory, see
-- http://cacm.acm.org/magazines/2014/5/174346-puzzled-a-sort-of-sorts/abstract

{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module SOS where

import SOS.State ( states, work )

import qualified SOS.Plain as P
import qualified SOS.Exp as E

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types hiding ( Var )
import Inter.Quiz

import qualified Data.Set as S
import Data.Typeable
import System.Random

data Sort_Of_Sorts = Sort_Of_Sorts deriving Typeable

data Instance p = Instance { cards :: Int } deriving Typeable

derives [makeToDoc,makeReader] [''Sort_Of_Sorts, ''Instance]

instance Show Sort_Of_Sorts where show = render . toDoc

instance OrderScore Sort_Of_Sorts where
    scoringOrder _ = Decreasing

instance Partial Sort_Of_Sorts (Instance P.Program) P.Program where
    describe _ i = vcat
        [ text "Gesucht ist ein speicherloses Sortierverfahren"
        , text "für" <+> toDoc (cards i) <+> text "Elemente in 3 Kellern."
        ]
    initial _ i = P.program0 $ cards i
    partial _ i p = return ()
    total _ i p = do      
        forM_ (states $ cards i) $ \ s -> do
            silent $ work P.step p S.empty s

make_fixed_plain = direct Sort_Of_Sorts 
                 $ (Instance { cards = 3 } :: Instance P.Program)
