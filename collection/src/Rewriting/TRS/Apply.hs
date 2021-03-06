{-# LANGUAGE TemplateHaskell #-}

module Rewriting.TRS.Apply where

import qualified Rewriting.Apply as A

import Rewriting.TRS
import Type.Tree

import Rewriting.Derive.Instance
import Rewriting.TRS.Step
import Rewriting.TRS.Steps

{-
import Rewriting.Derive.Quiz
import Rewriting.Derive.Config
-}

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

{-
import Autolib.FiniteMap
import Challenger.Partial
import Inter.Types
import Inter.Quiz
-}

import Control.Monad
import Data.Typeable

data For_TRS = For_TRS
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc ] [''For_TRS])

instance  A.Apply For_TRS ( TRS Identifier Identifier ) 
                       ( Term Identifier Identifier ) 
                       ( Step Identifier Identifier ) where
    example_object_of_size tag s = 
        read "f(a,f(a,b))" -- FIXME
    example tag = Instance
        { system = Rewriting.TRS.example
        , derivation_restriction = Length GT 2
        , from = Sized GT 0
        , to = Fixed $ read "f(f(b,a),a)"
        }
    apply tag system object action = do
        exec system object action
    actions tag system object = 
        steps system object

-- local variables:
-- mode: haskell
-- end:
