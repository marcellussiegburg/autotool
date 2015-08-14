{-# language MultiParamTypeClasses #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module Geo.Domain where

import qualified Prelude
import Prelude (Int)
import Polynomial.Class
import Prelude (succ, ($), (++), show)

import Polynomial.Type
import Autolib.TES.Identifier
import System.Random
import Control.Applicative

class Field k => Domain s k where
  fresh :: Integer -> s -> (k, s)

instance RandomGen g => Domain g Rational where
  fresh bnd g0 =
    let (p, g1) = randomR (negate bnd, bnd) g0
        (q, g2) = randomR (1, bnd) g1
    in  (p % q, g2)
        
instance Domain Int (Ratio (Poly Integer Identifier)) where
  fresh _ s0 = ( (variable $ mk 0 $ "f" ++ show s0)
                 % fromInteger 1
               , succ s0 )
    
