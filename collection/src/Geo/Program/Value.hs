{-# language TemplateHaskell #-}

module Geo.Program.Value where

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

data Value k
    = Boolean k -- ^ property is true iff k == 0
    | Number k  
    | Point (k,k)
    | Line (k,k,k)
    | Circle (k,k,k,k)  
    | Function Type [ Type ] ( [Value k] -> Eval k (Value k) )

data Type = BooleanT | NumberT
          | PointT | LineT | CircleT
          | FunctionT Type [Type]
    deriving (Eq)

type Env n d = M.Map n (Value d)

-- | the evaluation monad:
-- name supply
-- accumulate non-degeneracy conditions
-- tracing
-- failure (e.g., variable not bound, type error)
type Eval k v = WriterT [ k ] (StateT Int  Reporter ) v

add_ndg k = tell [k]

fresh :: Eval k Identifier
fresh = do s <- get ; put $ succ s ; return $ mk 0 $ "$" ++ show s

derives [makeToDoc] [''Value,''Type]
