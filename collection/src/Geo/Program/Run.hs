module Geo.Program.Run where

import Prelude hiding
  ( Num (..), Rational(..), Integer(..) )
import Polynomial.Class
import Polynomial.Type

import Geo.Program.AST
import Geo.Program.Value
import Geo.Program.Eval
import Geo.Domain
import Geo.Program.Ops
import Geo.Program.ToDoc

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.TES.Identifier

import Control.Monad.State
import Control.Monad.Writer
import System.Random

concrete
  :: ( RandomGen g, ToDoc g )
     => g 
     -> Exp Identifier
     -> Reporter ( Value Rational g , Trace Rational )
concrete g p = do
  evalStateT ( runWriterT (eval std p) ) g 
    
symbolic
  :: Exp Identifier
     -> Reporter ( Value (Ratio (Poly Integer Identifier)) Int,
                   Trace (Ratio (Poly Integer Identifier)) )
symbolic p = do
  evalStateT ( runWriterT (eval std p) ) 0  

