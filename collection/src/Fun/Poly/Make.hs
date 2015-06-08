module Fun.Poly.Make where

import Inter.Types

import Fun.Poly.Type
import Fun.Poly.Check
import Fun.Poly.Machine
import Fun.Poly.Examples

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I

import qualified Autolib.Reporter.Checker as R

import Autolib.Set

make :: Make
make = M.make $ C.Config
       { C.name = "Fun"
       , C.conditions = [ ]
       , C.arity = 2
       , C.op = read "x1 + x2" 
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = [ Builtins [ Plus, Minus ]  ]
       , C.start = plus :: Fun
       }
