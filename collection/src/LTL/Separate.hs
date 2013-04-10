{-# language TemplateHaskell, DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}

module LTL.Separate (make_fixed) where

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

data LTL_Separate = LTL_Separate 
  deriving (Typeable, Show)

derives [makeReader, makeToDoc] [''LTL_Separate]

instance OrderScore LTL_Separate where
    scoringOrder _ = None

data Instance = 
     Instance { w_true  :: ORW Assign
              , w_false :: ORW Assign
              , allowed_temporal_unary :: S.Set Uop
              , allowed_temporal_binary :: S.Set Bop
              }
    deriving ( Typeable )

example_instance :: Instance
example_instance =
    Instance { w_true = read "{p=True}^omega"
             , w_false = read "{p=False}^omega"
             , allowed_temporal_unary 
               = S.fromList [ Always, Eventually ]
             , allowed_temporal_binary 
               = S.fromList [ Until ]
             }

derives [makeReader, makeToDoc] [''Instance]

instance Partial LTL_Separate Instance Formula where
    describe _ i = vcat
        [ text "Gesucht ist eine PLTL-Formel,"
        , text "für die folgendes Omega-Wort ein Modell ist:"
        , nest 4 $ toDoc $ w_true i
        , text "und folgendes Omega-Wort kein Modell ist:"
        , nest 4 $ toDoc $ w_false i        
        , text "Erlaubt sind diese temporalen Operatoren:"
        , nest 4 $ text "unär:" 
           <+> toDoc (allowed_temporal_unary i)
        , nest 4 $ text "binär:" 
           <+> toDoc (allowed_temporal_binary i)
        ]

    initial _ i = 
       read "p || F q"

    partial _ i f = silent $ do
        let ups = S.fromList $ temporal_uops f
        inform $ text "verwendete unäre temporale Operatoren:" <+> toDoc ups
        assert ( S.isSubsetOf ups $ allowed_temporal_unary i ) $ text "alle sind erlaubt?"

        let bps = S.fromList $ temporal_bops f
        inform $ text "verwendete binäre temporale Operatoren:" <+> toDoc bps
        assert ( S.isSubsetOf bps $ allowed_temporal_binary i ) $ text "alle sind erlaubt?"

    total _ i f = do
        inform $ vcat 
               [ text "Wert der Formel für"
               , nest 4 $ toDoc $ w_true i ]
        v <- evaluate f $ w_true i
        assert v $ text "ist True?"
        inform $ vcat 
               [ text "Wert der Formel für"
               , nest 4 $ toDoc $ w_false i ]
        v <- evaluate f $ w_false i
        assert (not v) $ text "ist False?"

make_fixed :: Make
make_fixed = direct LTL_Separate example_instance