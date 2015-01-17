-- | nested univariate polynomials.
-- with extension (experimental) for weighted lex orders.

{-# language DeriveDataTypeable #-}

module Polynomial.Tree.Data where

import Prelude hiding (Num (..), (/), Integer, map, null)
import qualified Prelude

import Polynomial.Class 
import Polynomial.Base hiding (var)
import qualified Polynomial.Base

import Autolib.TES.Identifier

import Data.Typeable
import Control.Lens ( (^.) )

import qualified Data.IntMap.Strict as M

data Poly r v
   = Zero -- ^ can only occur at the very top (no subtree is Zero)
   | Number ! r  -- ^ must be nonzero
   | Branch { _weight :: ! Int
            , var :: ! v
            , children :: ! (M.IntMap (Poly r v))
            }
     -- ^ the key is the exponent.
   -- variables in the subtrees are smaller than variable in root.
    -- Not all variables have to be present on each path  (BDD-like).
     -- A Branch must contain the variable non-trivially
    -- (Branch v (M.singleton 0 _)) is forbidden 
   deriving (Typeable, Eq, Ord )

weight :: Poly r v -> Int
weight p = case p of
  Zero {} -> 0 -- questionable
  Number {} -> 0
  Branch {} -> _weight p

valid p = case p of
  Zero -> True
  Number r -> r /= zero
  Branch {} ->
       not (any null $ M.elems $ children p)
    && not (M.null $ M.delete 0 $ children p) --  variable must occur nontrivially   
    && all ( \ q -> case q of
                Branch {} -> var p > var q ; Number _ -> True
           ) (M.elems $ children p) 

-- | smart constructors 

number :: Ring r => r -> Poly r v
number r = if r == zero then Zero else Number r

branch :: v -> M.IntMap (Poly r v) -> Poly r v
branch v m =
  let m' = M.filter ( not . null ) m
  in  if M.null m' then Zero
      else if 1 == M.size m' && M.member 0 m'
           then m' M.! 0
           else Branch { var = v , children = m'
                       , _weight = maximum
                             $ fmap (\(k,v) -> k + weight v)
                             $ M.toList m'
                       }

absolute :: Ring r => Poly r v -> r
absolute p = case p of
  Zero -> zero
  Number r -> r
  Branch {} -> maybe zero absolute $ M.lookup 0 $ children p

null Zero = True ; null _ = False

map f p = case p of
  Zero -> Zero
  Number r -> number $ f r
  Branch {} -> branch (var p) $ M.map (map f) $ children p

divF p f = map ( / f ) p

terms :: Ord v => Poly r v -> [ Term r v ]
terms p = case p of
  Zero -> []
  Number r -> [ (r, mono []) ]
  Branch {} -> do
    (e, q) <- M.toDescList $ children p
    (c, m) <- terms q
    return (c, monoMult m $ mono [ factor (var p) e | e > 0 ] )

splitLeadingLex p = case p of
  Zero -> Nothing
  Number r -> return (( r, mono [] ), Zero )
  Branch {} -> do 
    ((e,q), m') <- M.maxViewWithKey $ children p
    ((c,l),r) <- splitLeadingLex q
    return ( (c, monoMult l $ mono [ factor (var p) e ])
           , branch (var p) $ M.insert e r m'
           )  

-- | TODO: make more efficient (if needed)
-- by storing this value in the nodes
nterms p = length $ terms p

constant :: Ring r => r -> Poly r v
constant r = number r

variable :: (Ord v, Ring r) => v -> Poly r v
variable v = monomial (mono [ factor v 1 ]) one

monomial :: Ring r => Mono v -> r -> Poly r v
monomial m c =
  foldr ( \ f p -> branch (f^. Polynomial.Base.var) $ M.singleton (f^.expo) p )
      (number c) (factors m)

compare_height (Branch {var=v} ) (Branch {var=w} ) =  compare v w
compare_height (Branch {} ) _ = GT
compare_height _ (Branch {}) = LT
compare_height _ _ = EQ

instance ( Ring r, Ord v) => Ring (Poly r v) where
    {-# specialize instance Ring (Poly Integer Identifier) #-}
    fromInteger i = constant $ fromInteger i
    zero = fromInteger 0 ; one = fromInteger 1
    negate = map negate

    Zero + q = q ; p + Zero = p
    p + q = case compare_height p q of
        GT -> branch (var p) $ M.insertWith (+) 0 q $ children p
        LT -> branch (var q) $ M.insertWith (+) 0 p $ children q
        EQ -> case (p,q) of
          (Branch {}, Branch {}) -> 
            branch (var p) $ M.unionWith (+) (children p)(children q)
          (Number r, Number s) -> number $ r + s  

    Zero * q = Zero ; p * Zero = Zero
    p * q = case compare_height p q of
        GT -> branch (var p) $ M.map (* q) $ children p
        LT -> branch (var q) $ M.map (p *) $ children q
        EQ -> case (p,q) of
          (Branch {} , Branch {} ) -> 
            branch (var p) $ M.fromListWith (+) $ do
              (c,f) <- M.toList $ children p
              (d,g) <- M.toList $ children q
              return (c+d, f*g)
          (Number r, Number s) -> number $ r * s  


