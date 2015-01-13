{-# language TemplateHaskell #-}

module Geo.Program.Value where

import Geo.Domain

import Geo.Program.AST (Kind (..))

import Control.Monad.Writer
import Control.Monad.State
import Autolib.Reporter

import Autolib.ToDoc
import Autolib.TES.Identifier ( mk, Identifier )
import qualified Data.Map.Strict as M


-- | cf. https://github.com/hg-graebe/GeoProver/blob/master/src/Inline/maxima.inline
--  Point A :== [a1,a2] <=> A=(a1,a2)
--  Line a :== [a1,a2,a3] <=> a1*x+a2*y+a3 = 0
--  Circle c :== [c0,c1,c2,c3] <=> c0*(x^2+y^2)+c1*x+c2*y+c3 = 0

data Value k s
    = Void
    | Boolean k -- ^ property is true iff k == 0
    | Number k  
    | Point (k,k)
    | Line (k,k,k)
    | Circle (k,k,k,k)
    | Angle (k,k)  
    | Function Type [ Type ]
      ( [Value k s] -> Eval k s (Value k s) )

data Type = VoidT | BooleanT | NumberT
          | PointT | LineT | CircleT | AngleT
          | FunctionT Type [Type]
    deriving (Eq)

type Env n d s = M.Map n (Value d s)

-- | the evaluation monad:
-- name supply
-- accumulate non-degeneracy conditions, preconditions, and claims
-- tracing
-- failure (e.g., variable not bound, type error)



type Trace k = [(Kind, k)]

type Eval k s v = WriterT (Trace k) (StateT s  Reporter ) v

add_ndg k = tell [(Prohibit, k)]

number :: Domain s d => Eval d s d
number = do
  s0 <- lift get
  let (k,s1) = fresh 30 s0
  lift $ put s1
  return k

-- Note: deriving for Value creates a bogus ToDoc s constraint
-- (a Function cannot be printed)

derives [makeToDoc] [''Value,''Type ]
