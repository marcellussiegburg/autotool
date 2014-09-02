{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language DeriveDataTypeable #-}


module Rewriting.TRS.Raw where

import Autolib.Symbol
import Autolib.TES.Term

import Autolib.TES.Rule
import Autolib.TES.Identifier
import qualified Autolib.TES

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

-- | this is the raw type,
-- needs to be processed because what should be variables
-- will be parsed as nullary symbols by the derived parser
data ( Symbol c, Symbol v ) => TRS v c = 
     TRS { variables :: [ v ]
         , rules :: [ Rule ( Term v c ) ]
         }
    deriving ( Eq, Ord, Typeable )



derives [makeReader, makeToDoc] [''TRS]

