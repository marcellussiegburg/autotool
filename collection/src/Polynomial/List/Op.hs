module Polynomial.List.Op where

import Polynomial.List.Data

import Polynomial.Class
import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Control.Lens
import Control.Applicative

import Control.Parallel.Strategies

import Autolib.TES.Identifier -- for specialization

{-# specialize merge :: [(Mono Identifier,Integer)] -> [(Mono Identifier,Integer)] -> [(Mono Identifier,Integer)] #-}
{-# specialize merge :: [(Identifier,Integer)] -> [(Identifier,Integer)] -> [(Identifier,Integer)] #-}

-- | largest comes first

merge xs ys = mrg xs ys `using` evalList rseq

mrg [] ys = ys ; mrg xs [] = xs
mrg (x:xs) (y:ys) = case compare (fst x) (fst y) of
  GT -> x : mrg xs (y:ys)
  LT -> y : mrg (x:xs) ys
  EQ -> let v = snd x + snd y
        in  if v == zero then mrg xs ys
            else (fst x, v) : mrg xs ys                  

foldB f z [] = z ; foldB f z [x] = x
foldB f z (x:y:zs) = foldB f z (zs ++ [f x y])

merges xss = foldB merge [] xss `using` evalList rseq

poly :: (Ord v, Ring r) => [(r, Mono v)] -> Poly r v
poly cms = foldB (+) zero
   $ map ( \(c,m) -> monomial m c ) cms

monomial m c = Poly
   { _unPoly = [(m,c) | c /= zero]
   , _absolute = if nullMono m then c else zero
   }

variable v = poly [ (one, mono [ factor v one ] ) ]

instance (Ring r, Ord v) => Ring (Poly r v) where
    {-# specialize instance Ring (Poly Integer Identifier) #-}
    fromInteger i = constant $ fromInteger i
    zero = fromInteger 0 ; one = fromInteger 1
    negate = over ( unPoly . mapped . _2 ) negate
           . over ( absolute ) negate  
    p + q = Poly
      { _absolute = p ^. absolute + q ^. absolute
      , _unPoly = merge (p ^. unPoly) (q ^.unPoly) 
      }
    p * q | length (p ^. unPoly) > length (q ^. unPoly) = Poly
      { _absolute = p ^. absolute * q ^. absolute
      , _unPoly = merges $ do
            (g,d) <- q ^. unPoly
            return $ map ( \ (f,c) -> (monoMult f g, c*d))
                   $ p ^. unPoly
      }      
    p * q = Poly
      { _absolute = p ^. absolute * q ^. absolute
      , _unPoly = merges $ do
            (f,c) <- p ^. unPoly
            return $ map ( \ (g,d) -> (monoMult f g, c*d))
                   $ q ^. unPoly
      }
    
monoMult p q = Mono 
    { _unMono = merge (p ^. unMono) (q ^. unMono) 
    , _total_degree = p ^. total_degree + q ^. total_degree
    }

splitAbsolute p =
  ( p ^. absolute , p - constant ( p^. absolute ) )

instance Normalize_Fraction (Poly r v) where
    -- RISKY?
    (%) = (:%)

divF :: Field f => Poly f v -> f -> Poly f v
divF p f =
   let d c = c / f
   in  over ( unPoly . mapped . _2 ) d
           $ over ( absolute ) d
           $ p




