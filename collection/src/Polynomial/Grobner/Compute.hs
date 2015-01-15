{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
{-# language DatatypeContexts #-}
{-# language NoMonomorphismRestriction #-}

module Polynomial.Grobner.Compute where

import qualified Prelude  
import Prelude
  hiding ( Num (..), Integer, null, negate, fromInteger)

import Polynomial.Class
import Polynomial.Type

import Autolib.Reporter
import Autolib.ToDoc

import Control.Applicative
import qualified Data.Set as S
import Control.Monad
import Data.Monoid
import Data.List ( inits, tails, nub )
import Data.Maybe
import System.IO

-- | reduce  polynomial g  w.r.t.  polynomials in f.
-- the leading term of the result
-- is not divisible by any leading term of f
reduce fs g = mfixpoint g $ reduce_step fs

reduce_step fs g = do
  (gc, gm) <- lm g
  msum $ for fs $ \ f -> do
    (fc,fm) <- lm f
    let (gc' :% fc') = gc % fc
    d <- divMono gm fm
    return $ constant fc' * g - monomial d gc' * f

-- | total-reduce  polynomial g  w.r.t.  polynomials in f.
-- no term of the result
-- is divisible by any leading term of f
total_reduce fs g = mfixpoint g $ total_reduce_step fs

total_reduce_step fs g = msum $ do
  (gc, gm) <- terms g
  f <- fs
  (fc,fm) <- maybeToList $ lm f
  let (gc' :% fc') = gc % fc
  return $ do
    d <- divMono gm fm
    return $ constant fc' * g - monomial d gc' * f

interreduce fs = mfixpoint fs $ \ fs -> msum $ do
    (pre, f : post ) <- splits fs
    return $ do
      f' <- reduce_step (pre ++ post) f
      return $ pre ++ f' : post

total_interreduce fs = mfixpoint fs $ \ fs -> msum $ do
    (pre, f : post ) <- splits fs
    return $ do
      f' <- total_reduce_step (pre ++ post) f
      return $ pre ++ f' : post

mfixpoint g op = case op g of
  Nothing -> g
  Just g' -> mfixpoint g' op

-- * computation

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

-- * autotool friendly Buchberger Algorithm

type Pair r v = (Poly r v, Poly r v)

pair p q = if p <= q then (p,q) else (q,p)

data (Ord v) => State v = State {
       -- | the current basis
        current :: S.Set (Poly Integer v)
         -- | pairs that need to be S-checked
         , todo :: S.Set (Pair Integer v)
         }

terse st = named_dutch_record (text "State")
  [ text "current" <+> toDoc (S.size $ current st)
  , text "todo" <+> toDoc (S.size $ todo st)
  ]

state0 fs = State
   { current = S.fromList fs
   , todo = S.fromList $ do f:gs <- tails fs ; map (pair f) gs
   }

data Option =
     Option { verbose :: Bool }

buchberger_step opt state = do
  inform $ if verbose opt then toDoc state
           else terse state
  let info d = when (verbose opt) $ inform d                
  case S.minView $ todo state of
    Nothing -> do
      info $ text "done"
      return Nothing
    Just ((f,g), odo) -> do
      info $ text "pair" </> toDoc (f,g)
      case spoly f g of
        Just s -> do
          info $ text "S polynomial" </> toDoc s
          let r = total_reduce (S.toList $ current state) s
          info $ text "after (total) reduction" </> toDoc r
          if null r then return $ Just $ state { todo = odo } else do
            let fresh = S.map (pair r) $ current state
            info $ text "fresh pairs" </> toDoc fresh
            return $ Just
                   $ state { todo = S.union fresh odo
                           , current = S.insert r $ current state
                           }
          
buchbergerIO fs = do
  let handle st = do
        let (Just res,msg :: Doc) = export $
               buchberger_step (Option{verbose=False}) st
        print msg
        case res of
          Nothing -> return $ S.toList $ current st
          Just st' -> handle st'
  handle $ state0 fs
  
for = flip map

splits xs = zip (inits xs) (tails xs)

derives [makeToDoc] [''State]


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

s1 :: [ Poly Integer Identifier ]
s1 = read " [ x^5 , x-1 ] "

