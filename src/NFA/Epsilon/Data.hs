{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module NFA.Epsilon.Data where

import Autolib.NFA ( NFAC )
import qualified Autolib.Relation
import Autolib.Set
import Autolib.FiniteMap

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data NFAC c s => ENFA c s = 
     ENFA { nfa_info   :: Doc 
         , alphabet :: Set c
         , states :: Set s
         , starts :: Set s
         , finals :: Set s
         , trans  :: FiniteMap (s, c) (Set s)
         , epsilon_trans :: Autolib.Relation.Type s s 
         }
    deriving Typeable

$(derives [makeToDoc, makeReader] [''ENFA])

