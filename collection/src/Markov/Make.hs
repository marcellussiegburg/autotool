{-# language MultiParamTypeClasses #-}

module Markov.Make where

import Inter.Types

import Markov.Type
import Markov.Machine

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I

import qualified Autolib.Reporter.Checker as R

make :: Make
make = M.make $ C.Config
       { C.name = "Markov"
       , C.conditions = [ ]
       , C.arity = 2
       , C.op = read "x1 + x2"
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = [()] 
       , C.start = Markov.Type.example
       }

instance C.Check () Program where
  check () p = return ()
