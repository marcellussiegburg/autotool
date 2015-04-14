module Goto.Make where

import Inter.Types

import Goto.Type
import Goto.Check
import Goto.Machine
import Goto.Property
import Goto.Example

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I

import qualified Autolib.Reporter.Checker as R

make :: Make
make = M.make $ C.Config
       { C.name = "Goto"
       , C.conditions = [ ]
       , C.arity = 2
       , C.op = read "x1 + x2"
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = Goto.Property.example
       , C.start = Goto.Example.student
       }
