module Compiler.Normalform where

import qualified While as W
import qualified Goto as G

import qualified Compiler.While_Goto as WG
import qualified Compiler.Goto_While as GW

normal :: W.Program -> W.Program
normal = GW.compile . WG.compile
