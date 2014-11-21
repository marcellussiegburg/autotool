{-# language NoMonomorphismRestriction #-}

module Rewriting.Abstract.Solve where

import qualified Autolib.Relation as R
import qualified Data.Set as S
import qualified OBDD as O
import qualified Data.Array as A

import Autolib.TES.Identifier
import Prelude hiding ( null )

-- * test case

test0 = do
    let name = read "R" :: Identifier
        u = unknown name 4
        g = O.not (cr u) O.&& wcr u
        vars = S.fromList $ for ( indices u ) $ \ (i,j) ->
             (name,i,j)
    print $ O.number_of_models vars g
    m <- O.some_model g
    print m

test1 = do
    let name = read "R" :: Identifier
        u = unknown name 3
        g = O.not (un u) O.&& un (inverse u)
        vars = S.fromList $ for ( indices u ) $ \ (i,j) ->
             (name,i,j)
    print $ O.number_of_models vars g
    m <- O.some_model g
    print m


-- * type

-- | this is OK but inefficient (?) since we have
-- independent BDDs while it should be a BDD base
type Rel from to v = A.Array (from,to) (O.OBDD v)

bounds  = A.bounds 
indices  = A.indices
get r (x, y) = r A.! (x,y)

unknown name n = farray ((1,1),(n,n)) $ \ (i,j) 
    -> O.unit (name,i,j) True

-- | dom and img must be consecutive
constant :: (A.Ix a, A.Ix b, Ord v)
         => R.Type a b -> Rel a b v
constant r = 
    let borders s = 
           (minimum $ S.toList s, maximum $ S.toList s)
        (a0,a1) = borders $ R.source r
        (b0,b1) = borders $ R.target r
    in  farray ((a0,b0),(a1,b1)) $ \ (i,j) -> 
            O.constant $ R.holds r i j

identic (lo,hi) = 
    constant $ R.identic $ S.fromList $ A.range (lo,hi)

-- * operations

complement r = farray (bounds r) $ \ i -> O.not $ get r i

inverse r = 
    let ((a0,b0),(a1,b1)) = bounds r
    in  farray ((b0,a0),(b1,a1)) $ \ (j,i) -> get r (i,j)

binary op r s = 
    farray (bounds r) $ \ i -> 
        O.binary op (get r i) (get s i)

union = binary (||)
difference = binary (>)
intersection = binary (&&)

times :: (A.Ix a, A.Ix b, A.Ix c, Ord v)
      => Rel a b v -> Rel b c v -> Rel a c v
times r s = 
    let ((a0,b0),(a1,b1)) = bounds r
        ((b0',c0),(b1',c1)) = bounds s
    in  farray ((a0,c0),(a1,c1)) $ \ (i,k) -> 
            O.or $ for (A.range (b0, b1)) $ \ j -> 
                get r (i, j) O.&& get s (j, k)

trans r = 
    let ((a0,b0),(a1,b1)) = bounds r
        dom = A.rangeSize (a0,a1) :: Int
        steps = truncate 
              $ ( logBase 2 $ fromIntegral dom :: Double )
        combine r = union r $ times r r
    in  iterate combine r !! steps

reflex r = 
    let ((a0,b0),(a1,b1)) = bounds r
    in  union (identic (a0,a1)) r

reflex_trans r = reflex $ trans r

bang r = 
    let t = reflex_trans r
        ((a0,_),(a1,_)) = bounds r
        normal = farray (a0,a1) $ \ i -> 
            O.and $ for (A.range(a0,a1)) $ \ j ->
                O.not $ get r (i, j)
    in  farray (bounds r) $ \ (i,j) ->
            get t (i,j) O.&& ( normal A.! j )

-- * properties

null r = O.not $ O.or $ for (indices r) $ \ i -> get r i
isSubsetOf r s = null $ difference r s
full r = null $ complement r
diagonal r = O.and $ for (indices r) $ \ (i,j) ->
    O.constant (i == j) O.|| (O.not $ get r (i,j))

un r = let b = bang r  in diagonal $ times (inverse b) b

cr r = 
    let fore = reflex_trans r ; back = inverse fore
    in  isSubsetOf (times back fore) (times fore back)

wcr r = 
    let fore = reflex_trans r ; back = inverse fore
    in  isSubsetOf (times (inverse r) r) (times fore back)

-- * convenience functions
-- | for  building arrays
farray :: A.Ix i => (i,i) -> (i -> a) -> A.Array i a
farray bnd f = 
    A.array bnd $ map ( \ i -> (i, f i)) $ A.range bnd

for = flip map
