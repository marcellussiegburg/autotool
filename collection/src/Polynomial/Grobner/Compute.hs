{- | Algorithms from

   @book{GBBIB529,
author = {B. Buchberger},
title = {Gr\"{o}bner-Bases: An Algorithmic Method in Polynomial Ideal Theory.},
length = {48},
language = {English},
chapter = {6},
pages = {184--232},
publisher = {Reidel Publishing Company, Dodrecht - Boston - Lancaster},
year = {1985},
refereed = {0},
book = {Multidimensional Systems Theory - Progress, Directions and Open Problems in Multidimensional Systems}
}

http://www.risc.jku.at/Groebner-Bases-Bibliography/details.php?details_id=529

-}

{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
{-# language DatatypeContexts #-}
{-# language NoMonomorphismRestriction #-}

module Polynomial.Grobner.Compute where

import qualified Prelude  
import Prelude
  hiding ( Num (..), div, gcd, Integer, null, negate, fromInteger)

import Polynomial.Class
import Polynomial.Type

import Autolib.Reporter
import Autolib.ToDoc

import Control.Applicative
import qualified Data.Set as S
import Control.Monad
import Data.Monoid
import Data.List ( inits, tails, nub, maximumBy )
import Data.Function (on)
import Data.Maybe
import System.IO

-- | Def 6.4   
spolynomial g h = maybe (error "spolynomial") id $ do
  (gc,gm) <- lm g
  (hc,hm) <- lm h
  let common = lcmMono gm hm
  gd <- divMono common gm
  hd <- divMono common hm
  let (gc' :% hc') = gc % hc
      r = monomial gd hc' * g - monomial hd gc' * h  
  return $ reduce_coefficients r

-- | divide polynomial by the gcd of its coefficients.  
reduce_coefficients p =
    let work g [] = g
        work g ((c,m):later) =
            let h = gcd g c
            in  if h == one then h else work h later
    in  case terms p of
          [] -> p
          (lc,lm):later -> 
            let g = work lc later
            in  if g == one then p else Polynomial.Type.map (\ c -> div c g) p
                    
-- | algorithm 6.1
normalform :: ( Ord v, Ring r, Normalize_Fraction r )
           => S.Set ( Poly r v ) -> Poly r v    -> Poly r v   
normalform f g =
  let candidates = do
          (gc,gm) <- terms g
          f <- S.toList f
          (fc,fm) <- lm f
          let (gc' :% fc') = gc % fc
          d <- divMono gm fm
          return (gm, constant fc' * g - monomial d gc' * f)
  in  if Prelude.null candidates then g
      else normalform f $ snd $ maximumBy (compare `on` fst) candidates
          
type Pair r v = (Poly r v, Poly r v)

pair p q = if p <= q then (p,q) else (q,p)

pairs ps qs = S.fromList $ do
    p <- S.toList ps ; q <- S.toList qs
    guard $ p /= q
    return $ pair p q

-- | algorithm 6.2 (straightforward computation, inefficient)
algorithm62 f = do
  let work (g,b) = case S.minView b of
          Nothing -> return g
          Just ((f1,f2), b' ) -> do
              let h = spolynomial f1 f2
              let h' = normalform g h
              inform $ vcat [ text "(f1,f2) =" <+> toDoc (f1,f2) , text "h =" <+> toDoc h, text "h' =" <+> toDoc h' ]
              if  null h'
              then work (g, b')
              else work (S.insert h' g, S.union b' $ pairs (S.singleton h') g )
  work (f, pairs f f)

  
data (Ord v) => State v = State {
       step :: Int
       -- | the current basis 
        , g :: S.Set (Poly Integer v)
         -- | pairs that need to be S-checked
         , b :: S.Set (Pair Integer v)
        -- | contains polynomials of G which can be reduced modulo the other polynomials of G
        , r :: S.Set (Poly Integer v)
        -- | and store the resulting reduced polynomials in P
        , p :: S.Set (Poly Integer v)
         }

terse st = named_dutch_record (text "State")
  [ text "step" <+> toDoc (step st)
  , text "g" <+> toDoc (S.size $ g st)
  , text "b" <+> toDoc (S.size $ b st)
    , text "r" <+> toDoc (S.size $ r st)
    , text "p" <+> toDoc (S.size $ p st)
  ]

state0 fs = State
   { step = 0 , r = S.fromList fs, p = S.empty , g = S.empty , b = S.empty }

count st = st { step = succ $ step st }
   
data Option =
     Option { max_steps :: Maybe Int
            , verbose :: Bool
            , criterion1 :: Bool
            , criterion2 :: Bool
            }

algorithm63 :: (ToDoc v, Ord v) => Option -> [ Poly Integer v ] -> Reporter (State v)
algorithm63 opt (fs :: [Poly Integer v]) = do
  let work s = do
        inform $ text "work" <+> toDoc s
        case S.minView (b s) of
          Nothing -> do
            return s
          Just ((f1,f2),b') -> do
            let h = normalform (g s) $ spolynomial f1 f2
            if null h
            then work $ count $ s { b = b' }
            else do
              let (g0, g1) = S.partition ( \ g -> lt g <= lt h ) (g s)
                  s' = count
                     $ s
                       { r = g0 , p = S.singleton h , g = g1
                       , b = S.filter ( \(f1,f2) -> S.notMember f1 g0 && S.notMember f2 g0) b'
                       }
              s0 <- reduce_all s'
              s1 <-  new_basis s0
              work s1
  s0 <- reduce_all (state0 fs)
  s1 <- new_basis s0
  work s1

new_basis :: (ToDoc v, Ord v) => State v -> Reporter (State v)
new_basis s = do
  inform $ text "new_basis" <+> toDoc s
  let gee = S.union (g s) (p s)
  return $ count $  s { g = S.map ( \ h -> normalform (S.delete h gee) h ) gee
        , b = S.union (b s) $ pairs gee (p s)
        -- r and p not changed?
        }

reduce_all :: (ToDoc v, Ord v) => State v -> Reporter (State v)  
reduce_all s = do
  inform $ text "reduce_all" <+> toDoc s
  case S.minView (r s) of
     Nothing -> return s
     Just (h0 , r') -> do
       let h = normalform (S.union (g s) (p s)) h0
       if  null h
       then reduce_all $ count $ s { r = r' }
       else do
         let (g0,g1) = S.partition (\ g -> lt h <= lt g ) (g s)
             (p0,p1) = S.partition (\ p -> lt h <= lt p ) (p s)
         reduce_all $ count
                    $ s { g = g1, p = S.insert h p1, r = S.unions [ g0, p0, r s ]
                       , b = S.filter ( \(f1,f2) -> S.notMember f1 g0 && S.notMember f2 g0) (b s)
                      }
  
  
buchbergerIO fs = do
   let (res,msg :: Doc) = export $ algorithm62 $ S.fromList fs
   print msg
   case res of
       Just g -> return g
  
for = flip fmap

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

