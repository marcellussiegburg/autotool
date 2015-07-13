module While.Make where

import Inter.Types

import While.Type
import While.Check
import While.Machine
import While.Property

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I

import qualified Autolib.Reporter.Checker as R

make :: Make
make = M.make $ C.Config
       { C.name = "While"
       , C.conditions = [ ]
       , C.arity = 2
       , C.op = read "x1 + x2"
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = While.Property.example
       , C.start = While.Type.example
       }
