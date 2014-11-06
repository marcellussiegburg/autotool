{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Fixed where

import Rewriting.Abstract.Rel

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Autolib.TES
import Autolib.TES.Identifier
import qualified Autolib.Relation as R

import Autolib.FiniteMap
import qualified Data.Map as M

import Challenger.Partial
import Inter.Types
import Data.Typeable


data Problem = 
     Problem { given :: M.Map Identifier Rel 
             , wanted :: S.Set Identifier
             , domain_size :: Maybe (Ordering, Int)
             , property :: Boolean Prop
             }
    deriving Typeable

derives [makeReader, makeToDoc] [''Problem]


