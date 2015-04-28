module Compiler.Normalform where

import qualified While as W
import qualified Goto as G

import qualified Compiler.While_Goto as WG
import qualified Compiler.Goto_While as GW

-- | konstruktiver Beweis für den Satz (von Kleene):
-- zu jedem While-Programm gibt es ein äquivalentes
-- mit genau einem While.

normal :: W.Program -> W.Program
normal = GW.compile . WG.compile
