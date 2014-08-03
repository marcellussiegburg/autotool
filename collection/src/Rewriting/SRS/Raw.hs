{-# LANGUAGE TemplateHaskell #-}

module Rewriting.SRS.Raw where

import Autolib.Symbol

import Autolib.TES.Rule
import Autolib.TES.Identifier
import qualified Autolib.TES

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable


data ( Symbol c ) => SRS c = 
     SRS { rules :: [ Rule [c] ]
         }
    deriving ( Eq, Ord, Typeable )

example :: SRS Identifier
example = read "SRS { rules = [ a b -> b a ] }"

$(derives [makeReader, makeToDoc] [''SRS])



-- local variables:
-- mode: haskell
-- end:
