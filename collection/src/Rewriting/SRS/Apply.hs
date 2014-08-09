{-# LANGUAGE TemplateHaskell #-}

module Rewriting.SRS.Apply where

import qualified Rewriting.Apply as A

import Rewriting.Derive.Instance
import Rewriting.SRS.Raw
import Rewriting.SRS.Step
import Rewriting.SRS.Steps

import Autolib.TES.Identifier
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader


import Control.Monad
import Data.Typeable

data For_SRS = For_SRS
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc ] [''For_SRS])

instance  A.Apply For_SRS ( SRS Identifier ) 
                       [ Identifier ]
                       ( Step Identifier ) where
    example_object_of_size tag s = 
        take s $ concat $ repeat $ read "[a,b,c]"
    example tag = Instance
        { system = Rewriting.SRS.Raw.example
        , derivation_restriction = Length GT 2
        , from = Fixed $ read "[a,a,b,b]"
        , to   = Sized GT 0
        }
    apply tag system object action = do
        exec system object action
    actions tag system object = 
        steps system object


