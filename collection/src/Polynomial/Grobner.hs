{-# language TupleSections #-}

module Polynomial.Grobner where

import qualified Prelude  
import Prelude
  hiding ( Num (..), Integer, null, negate, fromInteger)

import Polynomial.Class
import Polynomial.Type

import Control.Applicative
import qualified Data.Set as S
import Control.Monad
import Data.Monoid
import Data.List ( inits, tails )
import Data.Maybe
import System.IO

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
-- always reduce a pair that has low lm (?)
--  http://arxiv.org/abs/1206.6940
buchberger fs =
  let handle done todo = case S.minView todo of
        Nothing -> done
        Just ((_,(g,h)), odo) -> case spoly g h of
          Nothing -> handle done odo
          Just s ->
            let r = reduce done s
            in  if null r
                then handle done odo
                else handle (r : done)
                   $ S.union odo
                   $ S.fromList $ map (pair r) done
      pair g h = (lcmMono <$> (lt g) <*> (lt h), (g,h))
  in handle fs $ S.fromList
               $ do g : hs <- tails fs
                    map (pair g) hs

buchbergerIO fs = do
  let pair g h = (lcmMono <$> (lt g) <*> (lt h), (g,h))
  let handle done todo = do
        putStr $ show (length done, S.size todo)
        hFlush stdout
        case S.minView todo of
         Nothing -> return done
         Just ((_,(g,h)), odo) -> case spoly g h of
          Nothing -> handle done odo
          Just s -> do
            let r = reduce done s
            if null r
                then handle done odo
                else handle (done ++ [r])
                   $ S.union odo
                   $ S.fromList $ map (pair r) done
  handle fs $ S.fromList
               $ do g : hs <- tails fs
                    map (pair g) hs

for = flip map

splits xs = zip (inits xs) (tails xs)

b2 :: [ Poly Integer Identifier ]
b2 = read "[ a * x - y^2, a * y - z^2, a * z - x^2 ]"

b3 :: [ Poly Integer Identifier ]
b3 = read "[ x^2+y+z-3, x+y^2+z-3, x+y+z^2-3 ]"
 
b5 :: [ Poly Integer Identifier ]
b5 = [ read "w + x + y +z"
     , read "w*x + x*y + y*z + z*w"
     , read "w*x*y + x*y*z + y*z*w + z*w*x"
     , read "w*x*y*z - 1"
     ]  

-- | http://www.sagemath.org/doc/reference/polynomial_rings/sage/rings/polynomial/multi_polynomial_ideal_libsingular.html

s1 :: [ Poly Integer Identifier ]
s1 = read " [ x^5 + y^4 + z^3 - 1,  x^3 + y^3 + z^2 - 1 ] "

s2 :: [ Poly Integer Identifier ]
s2 = read " [x^2 - 2*y^2, x*y - 3] "
