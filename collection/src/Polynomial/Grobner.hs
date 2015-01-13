{-# language TupleSections #-}

module Polynomial.Grobner where

import qualified Prelude  
import Prelude
  hiding ( Num (..), Integer, null, negate, fromInteger)

import Polynomial.Class
import Polynomial.Type

import Control.Monad
import Data.Monoid
import Data.List ( inits, tails )
import Data.Maybe

-- | reduce  polynomial g  w.r.t.  polynomials in f.
-- the leading term of the result
-- is not divisible by any leading term of f
reduce fs g = mfixpoint g $ reduce_step fs

mfixpoint g op = case op g of
  Nothing -> g
  Just g' -> mfixpoint g' op

reduce_step fs g = do
  (gc, gm) <- lm g
  msum $ for fs $ \ f -> do
    (fc,fm) <- lm f
    let (gc' :% fc') = gc % fc
    d <- divMono gm fm
    return $ constant fc' * g - monomial d gc' * f

interreduce fs = mfixpoint fs $ \ fs -> msum $ do
    (pre, f : post ) <- splits fs
    return $ do
      f' <- reduce_step (pre ++ post) f
      return $ pre ++ f' : post
   
spoly g h = do
  (gc,gm) <- lm g
  (hc,hm) <- lm h
  let common = lcmMono gm hm
  gd <- divMono common gm
  hd <- divMono common hm
  let (gc' :% hc') = gc % hc
  return $ monomial gd hc' * g - monomial hd gc' * h

-- | Computes a Grober basis for the ideal of fs.
-- Uses the term ordering that is implicit in splitLeading.
buchberger fs =
  let handle done [] = done
      handle done ((g,h):odo) = case spoly g h of
        Nothing -> handle done odo
        Just s ->
          let r = reduce done s
          in  if null r
              then handle done odo
              else handle (r : done)
                   $ map (,r) done ++ odo        
  in handle fs $ do g : hs <- tails fs ; map (g,) hs

for = flip map

splits xs = zip (inits xs) (tails xs)

b2 :: [ Poly Integer Identifier ]
b2 = read "[ a * x - y^2, a * y - z^2, a * z - x^2 ]"

b3 :: [ Poly Integer Identifier ]
b3 = read "[ x^2+y+z-3, x+y^2+z-3, x+y+z^2-3 ]"
 
