-- | nested univariate polynomials

module Polynomial.Tree.Data where

import Prelude hiding (Num (..), (/), Integer, map, null)
import qualified Prelude

import Polynomial.Class 
import Polynomial.Base

import Autolib.TES.Identifier

import Data.Typeable
import Control.Lens

import qualified Data.IntMap.Strict as M

import Test.Hspec.Core.Runner ( hspec )

data Poly r v
   = Zero
   | Number ! r  -- ^ nonzero
   | Branch ! v ! (M.IntMap (Poly r v))
     -- ^ the key is the exponent.
   -- variables in the subtrees are smaller than variable in root.
    -- Not all variables have to be present on each path  (BDD-like).
     -- A must contain the variable non-trivially
    -- (Branch v (M.singleton 0 _)) is forbidden 
   deriving (Eq )

valid p = case p of
  Zero -> True
  Number r -> r /= zero
  Branch v m ->
       not (M.null m)
    && not (any null $ M.elems m)
    && not (M.null $ M.delete 0 m) -- ^ variable must occur nontrivially   
    && all ( \ q -> case q of
                Branch w _ -> v > w ; Number _ -> True
           ) (M.elems m) 

-- | smart constructors 

number r = if r == zero then Zero else Number r

branch v m =
  let m' = M.filter ( not . null ) m
  in  if M.null m' then Zero
      else if 1 == M.size m' && M.member 0 m'
           then m' M.! 0
           else Branch v m'

absolute :: Ring r => Poly r v -> r
absolute p = case p of
  Zero -> zero
  Number r -> r
  Branch v m -> maybe zero absolute $ M.lookup 0 m 

null Zero = True ; null _ = False

map f p = case p of
  Zero -> Zero
  Number r -> number $ f r
  Branch v m -> branch v $ M.map (map f) m

divF p f = map ( / f ) p

terms :: Ord v => Poly r v -> [ Term r v ]
terms p = case p of
  Zero -> []
  Number r -> [ (r, mono []) ]
  Branch v m -> do
    (e, q) <- M.toDescList m
    (c, m) <- terms q
    return (c, monoMult m $ mono [ factor v e | e > 0 ] )

-- | TODO: make more efficient (if needed)
-- by storing this value in the nodes
nterms p = length $ terms p

constant :: Ring r => r -> Poly r v
constant r = number r

variable :: (Ord v, Ring r) => v -> Poly r v
variable v = monomial (mono [ factor v 1 ]) one

monomial :: Ring r => Mono v -> r -> Poly r v
monomial m c =
  foldr ( \ f p -> branch (f^.var) $ M.singleton (f^.expo) p )
      (number c) (factors m)

compare_height (Branch v _ ) (Branch w _ ) =  compare v w
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
        GT -> let Branch pv pm = p
              in branch pv $ M.insertWith (+) 0 q pm
        LT -> let Branch qv qm = q
              in branch qv $ M.insertWith (+) 0 p qm
        EQ -> case (p,q) of
          (Branch pv pm, Branch qv qm) -> 
            branch pv $ M.unionWith (+) pm qm
          (Number r, Number s) -> number $ r + s  

    Zero * q = Zero ; p * Zero = Zero
    p * q = case compare_height p q of
        GT -> let Branch pv pm = p
              in branch pv $ M.map (* q) pm
        LT -> let Branch qv qm = q
              in branch qv $ M.map (p *) qm
        EQ -> case (p,q) of
          (Branch pv pm, Branch qv qm) -> 
            branch pv $ M.fromListWith (+) $ do
              (c,f) <- M.toList pm
              (d,g) <- M.toList qm
              return (c+d, f*g)
          (Number r, Number s) -> number $ r * s  

instance Normalize_Fraction (Poly r v) where
    -- RISKY?
    (%) = (:%)

-- | this is generic (and should be in a different module)
poly :: (Ring r, Ord v) => [ Term r v ] -> Poly r v
poly cms = foldl (+) zero
    $ Prelude.map ( \ (c,m) -> monomial m c ) cms

splitAbsolute p = ( absolute p, p - constant (absolute p))
