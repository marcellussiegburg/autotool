{-# language NoMonomorphismRestriction #-}

module Polynomial.List.Op
( module Polynomial.List.Data
, module Polynomial.List.Op
) where

import Polynomial.List.Data

import Polynomial.Class
import Polynomial.Base
import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Control.Lens
import Control.Applicative

import Control.Parallel.Strategies
import Control.DeepSeq

import Autolib.TES.Identifier -- for specialization
import Autolib.Hash (hash)

instance NFData Identifier where
  rnf i = hash i `seq` show i `seq` ()

{-# specialize merge :: [(Mono Identifier,Integer)] -> [(Mono Identifier,Integer)] -> [(Mono Identifier,Integer)] #-}
{-# specialize merge :: [(Identifier,Integer)] -> [(Identifier,Integer)] -> [(Identifier,Integer)] #-}


foldB :: (a -> a -> a) -> a -> [a] -> a
foldB f z [] = z
foldB f _ xs =
  let work (x:y:zs) done =
        let z = f x y in z `seq` work zs (z : done)
      work [x] [] = x
      work xs done = work (xs ++ done) []
  in  work xs []

merges xss = foldB merge [] xss 

poly :: ( -- NFData v, NFData r,
  Ord v, Ring r) => [(r, Mono v)] -> Poly r v
poly cms = foldB (+) zero
   $ map ( \(c,m) -> monomial m c ) cms

monomial m c = Poly
   { unPoly = [(m,c) | c /= zero]
   , absolute = if nullMono m then c else zero
   }

variable v = poly [ (one, mono [ factor v one ] ) ]

instance ( Ring r, Ord v) => Ring (Poly r v) where
    {-# specialize instance Ring (Poly Integer Identifier) #-}
    fromInteger i = constant $ fromInteger i
    zero = fromInteger 0 ; one = fromInteger 1
    negate p = p { absolute = negate $ absolute p
                 , unPoly = over ( mapped . _2 ) negate
                            $ unPoly p
                 }
    p + q = Poly
      { absolute = absolute p + absolute q
      , unPoly = merge (unPoly p) (unPoly q) 
      }
    p * q | length (unPoly p) > length (unPoly q) = Poly
      { absolute = absolute p * absolute q
      , unPoly = merges $ do
            (g,d) <- unPoly q
            return $ map ( \ (f,c) -> (monoMult f g, c*d))
                   $ unPoly p
      }      
    p * q = Poly
      { absolute = absolute p *  absolute q
      , unPoly = merges $ do
            (f,c) <- unPoly p 
            return $ map ( \ (g,d) -> (monoMult f g, c*d))
                   $ unPoly q
      }
    


splitLeading p = case unPoly p of
  [] -> Nothing
  (m,c) : rest ->
    Just ((c,m), Poly { unPoly = rest
                      , absolute = case rest of
                        [] -> zero
                        _ -> absolute p
                      } )

instance Normalize_Fraction (Poly r v) where
    -- RISKY?
    (%) = (:%)

divF :: Field f => Poly f v -> f -> Poly f v
divF p f =
   let d c = c / f
   in  p { absolute = d $ absolute p
               , unPoly = over (  mapped . _2 ) d
                          $ unPoly p
               }




