module Fourier_Motzkin.Solve where

import Fourier_Motzkin.Type

import Data.List ( partition )
import qualified Data.Set as S
import qualified Data.Map as M

-- |   f == 0 , where f contains x, 
--  is transformed to   f' == x , where f' does not contain x
remove x atom = 
    let c = coefficient x $ linear atom
        s = scale (negate $ 1 / c) $ linear atom
    in  removeKey x s

resolve ::Ord v => v -> Constraint v -> Constraint v
resolve x con =
    let (withx, nox) = partition (contains x . linear) con
        (posx, negx) = partition ((> 0) . coefficient x . linear) withx
    in  nox ++ do
            p <- posx ; q <- negx
            return $ atom (strict p || strict q) 
                   $ minus (remove x q) (remove x p)

satisfiable con = 
    let vs = S.unions $ map (variables . linear) con
    in  case S.minView vs of
            Nothing -> all ( \ a -> let cmp = if strict a then (> 0) else (>= 0)
                                    in  cmp $ absolute $ linear a ) con
            Just (x,xs) -> satisfiable $ resolve x con

unsat = 
    [ NonNegative $ Linear $ M.fromList [(Nothing, 1),(Just "x",-1), (Just "y", -1)] 
    , NonNegative $ Linear $ M.fromList [(Nothing, -1/2),(Just "x",1)]
    , NonNegative $ Linear $ M.fromList [(Nothing, -3/5),(Just "y",1)]
    ]


