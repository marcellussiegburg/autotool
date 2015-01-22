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


module Polynomial.Grobner.Compute where

import qualified Prelude  
import Prelude
  hiding ( Num (..), div, gcd, Integer, null, negate, fromInteger)

import Polynomial.Class
import Polynomial.Type

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.TES.Identifier (mk)

import Control.Applicative
import qualified Data.Set as S
import Control.Monad
import Data.Monoid
import Data.List ( inits, tails, nub, maximumBy )
import Data.Function (on)
import Data.Maybe
import System.IO

-- | Def 6.4
spolynomial :: (Euclidean_Ring r , Normalize_Fraction r, Ord v) =>  Poly r v -> Poly r v -> Poly r v   
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

-- | the @tag@ is there for sorting,
--   so that S.minView below picks
--   "a pair in B whose LCM(..) is minimal"
data (Ring r, Ord r, Ord v) => Pair r v = Pair { tag :: Mono v
     , first :: Poly r v, second :: Poly r v
     } deriving (Eq, Ord)

pair p q =
     let c = lcmMono (the $ lt p) (the $ lt q)
     in  if p <= q then Pair c p q else Pair c q p

pairs ps qs = S.fromList $ do
    p <- S.toList ps ; q <- S.toList qs
    guard $ p /= q
    return $ pair p q

-- | algorithm 6.2 (straightforward computation, inefficient)
algorithm62 fs = do
  let work (g,b) = case S.minView b of
          Nothing -> return g
          Just (p, b' ) -> do
              let f1 = first p ; f2 = second p
                  h = spolynomial f1 f2
              let h' = normalform g h
              inform $ vcat [ text "pair =" <+> toDoc p , text "h =" <+> toDoc h, text "h' =" <+> toDoc h' ]
              if  null h'
              then work (g, b')
              else work (S.insert h' g, S.union b' $ pairs (S.singleton h') g )
  let f = S.fromList fs
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
        , reduction_count :: Int
        , trace :: [(Int, Poly Integer v)]
         }

oneline = text . unwords . words . show
     
terse st = oneline $ named_dutch_record (text "State")
  [ text "step" <+> toDoc (step st)
  , text "g" <+> toDoc (S.size $ g st)
  , text "b" <+> toDoc (S.size $ b st)
  , text "r" <+> toDoc (S.size $ r st)
  , text "p" <+> toDoc (S.size $ p st)
  , text "reduction_count" <+> toDoc (reduction_count st)
  ]

state0 fs = State
   { step = 0 , r = S.fromList fs, p = S.empty , g = S.empty , b = S.empty, reduction_count = 0, trace = [] }

count st = st { step = succ $ step st }
countred st = st { reduction_count = succ $ reduction_count st }
   
data Option =
     Option { max_steps :: Maybe Int
            , verbose :: Bool
            , use_criterion1 :: Bool
            , use_criterion2 :: Bool
            }

option0 = Option
    { max_steps = Just 100
    , verbose = False
    , use_criterion1 = True
    , use_criterion2 = True
    }

info opt d = when (verbose opt) $ inform d     
     
algorithm63 :: (ToDoc v, Ord v)
            => Option -> [ Poly Integer v ]
            -> Reporter [Poly Integer v]
algorithm63 opt (fs :: [Poly Integer v]) = do
  s <- algorithm63s opt fs
  return $ S.toList $ g s

algorithm63s opt fs = do  
  let w_nb_ra s = do reduce_all opt s >>= new_basis opt >>= work
      work s = do
        inform $ text "work" <+> if verbose opt then toDoc s else terse s        
        case S.minView (b s) of
          Just (p,b') -> do
            case max_steps opt of
                Just ms -> when (step s>ms) $ reject
                     $ text "too many steps"
            let f1 = first p ; f2 = second p
            info opt $ text "pair =" <+> toDoc p
            let c1 = criterion1 p s
                c2 = criterion2 p
                h = normalform (g s) $ spolynomial f1 f2
            info opt $ hsep [ text "c1 =", toDoc c1 , text "c2 =", toDoc c2 ]
            if  c1 || c2 || null h -- evaluate h lazily (criteria first)
            then do
              info opt $ text "is trivial"
              work $ count $ s { b = b' }
            else do
              info opt $ text "h =" <+> toDoc h
              let (g0, g1) = S.partition ( divides h ) (g s)
              w_nb_ra $ count
                     $ s
                       { r = g0 , p = S.singleton h , g = g1
                       , b = S.filter ( \ p -> S.notMember (first p) g0 && S.notMember (second p) g0) b'
                       , trace = (step s, h):trace s
                       }
          Nothing -> do
            return s
  w_nb_ra $ state0 fs

criterion1 f s = or $ do
    let f1 = first f ; f2 = second f
    p <- S.toList $ g s
    return $ and
           [ p /= f1 , p /= f2
           , dividesMono (the $ lt p) $ tag f
           , S.notMember (pair f1 p) $ b s
           , S.notMember (pair f2 p) $ b s
           ]
  
criterion2 pa = tag pa == multMono (the $ lt $ first pa) (the $ lt $ second pa)

divides p q = the (lt p) `dividesMono` the (lt q)
the m = maybe (error "the") id m
        
new_basis :: (ToDoc v, Ord v) => Option -> State v -> Reporter (State v)
new_basis opt s = do
  info opt $ text "new_basis" <+> toDoc s
  let gee = S.union (g s) (p s)
  return $ count $  s { g = S.map ( \ h -> normalform (S.delete h gee) h ) gee
        , b = S.union (b s) $ pairs gee (p s)
        -- r and p not changed?
        }

reduce_all :: (ToDoc v, Ord v) => Option -> State v -> Reporter (State v)  
reduce_all opt (s :: State v) = do
  info opt $ text "reduce_all" <+> toDoc s
  case S.minView (r s) of
     Nothing -> return s
     Just (h0 , r') -> do
       let h = normalform (S.union (g s) (p s)) h0
       info opt $ text "inside:" <+> vcat [ text "h0 =" <+> toDoc h0 , text "h =" <+> toDoc h ]
       if  null h
       then do
         reduce_all opt $ count $ s { r = r' }
       else do
         let (g0,g1) = S.partition ( divides h ) (g s)
             (p0,p1) = S.partition ( divides h ) (p s)
         reduce_all opt $ count
                    $ s { g = g1, p = S.insert h p1, r = S.unions [ g0, p0, r' ]
                       , b = S.filter ( \ p -> S.notMember (first p) g0 && S.notMember (second p) g0) (b s)
                      }
  
  
buchbergerIO fs = do
   let (res,msg :: Doc) = export
           -- $ algorithm62 
           $ algorithm63 ( Option { verbose = False } )
           $ fs
   print msg
   case res of
       Just g -> return g
  
for f xs = flip fmap f xs

splits xs = zip (inits xs) (tails xs)

derives [makeToDoc] [''Pair, ''State]

s1 :: [ Poly Integer Identifier ]
s1 = read " [ x^5 , x-1 ] "

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

ex66 :: [ Poly Integer Identifier ]
ex66 = read "[ x^3*y*z - x*z^2 , x*y^2*z-x*y*z, x^2*y^2-z^2 ]"

cyc :: Int -> [ Poly Integer Identifier ]
cyc n = do
  let vs = do c <- [ 'a' .. ] ; return $ mk 0 [c]
  deg <- [ 1 .. n ]
  return $ poly
         $ ( if deg == n
             then (( negate $ Prelude.fromIntegral n,  mono[]): )
             else id)
         $ do
       s <- [0 ..  n-1]
       return $ (one, ) $ mono $ do
           o <- [ 0 .. deg-1 ]
           return $ factor (vs !! Prelude.mod (s+o) n) 1

