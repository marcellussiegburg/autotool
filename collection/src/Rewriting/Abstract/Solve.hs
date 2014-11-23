{-# language NoMonomorphismRestriction #-}

module Rewriting.Abstract.Solve where

import qualified Rewriting.Abstract.Data as D

import qualified Autolib.Relation as R
import qualified Data.Set as S
import qualified OBDD as O
import qualified Data.Array as A
import qualified Data.Map as M

import Autolib.TES.Identifier
import Prelude hiding ( null )

-- * interface

solvable unknowns prop dom =
    let env = M.fromList $ for unknowns $ \ u -> 
             ( u, unknown u dom )
    in  O.satisfiable $ property env prop

expression env exp = case exp of
    D.ExpParens x -> expression env x
    D.Ref i -> env M.! i
    D.Op1 op x -> 
        let v = expression env x
        in  case op of
            D.Inverse -> inverse v
            D.Complement -> complement v
            D.Transitive_Closure -> transitive_closure v
            D.Transitive_Reflexive_Closure -> 
                reflexive_transitive_closure v
    D.Op2 op x y ->
        let v = expression env x
            w = expression env y
        in  case op of
            D.Union -> union v w
            D.Intersection -> intersection v w
            D.Difference -> difference v w
            D.Product -> times v w

property env prop = case prop of
    D.PropParens p -> property env p
    D.And ps -> O.and $ map (property env) ps
    D.Or ps -> O.or $ map (property env) ps
    D.Not p -> O.not $ property env p
    D.Prop1 p x ->
        let r = expression env x
        in  case p of
            D.Null -> null r
            D.Full -> full r
            D.Reflexive -> reflexive r
            D.Irreflexive -> irreflexive r
            D.Transitive -> transitive r
            D.Symmetric -> symmetric r
            D.Antisymmetric -> antisymmetric r
            D.Asymmetric -> asymmetric r
            D.Total -> total r
            D.SN -> sn r
            D.WN -> wn r
            D.CR -> cr r
            D.WCR -> wcr r
            D.UN -> un r
            D.UNC -> unc r
    D.Prop2 p x y ->
        let r = expression env x
            s = expression env x
        in  case p of
            D.Equals -> equals r s
            D.Subsetof -> isSubsetOf r s
            D.Disjoint -> disjoint r s


-- * test cases

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

test2 = do
    let name = read "R" :: Identifier
        u = unknown name 5
        g = O.and [ O.constant True
                  -- , wcr u
                  , un u
                  , O.not $ unc u 
                  ]
        vars = S.fromList $ for ( indices u ) $ \ (i,j) ->
             (name,i,j)
    print $ O.number_of_models vars g
    m <- O.some_model g
    print m


test3 = do
    let name = read "R" :: Identifier
        u = unknown name 3
        g = O.and [ O.constant True
                  , irreflexive u
                  , antisymmetric u
                  , O.not $ sn u
                  ]
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

transitive_closure = transitive_closure_binary

transitive_closure_binary r = 
    let ((a0,b0),(a1,b1)) = bounds r
        dom = A.rangeSize (a0,a1) :: Int
        steps = ceilBase 2 dom
        combine r = union r $ times r r
    in  iterate combine r !! steps

ceilBase b x = if x == 1 then 0 
    else let (d,m) = divMod x b
         in  succ $ ceilBase b $ if m == 0 then d else succ d

transitive_closure_unary r = 
    let ((a0,b0),(a1,b1)) = bounds r
        dom = A.rangeSize (a0,a1) :: Int
        steps = dom
        combine s = union s $ times s r
    in  iterate combine r !! steps



symmetric_closure r = 
    farray (bounds r) $ \ (i,j) -> 
        get r (i,j) O.|| get r (j,i)

reflexive_closure r = 
    let ((a0,b0),(a1,b1)) = bounds r
    in  union (identic (a0,a1)) r

reflexive_transitive_closure r = 
    reflexive_closure $ transitive_closure r

symmetric_reflexive_transitive_closure r = 
    reflexive_transitive_closure $ symmetric_closure r

bang r = 
    let t = reflexive_transitive_closure r
        ((a0,_),(a1,_)) = bounds r
        normal = farray (a0,a1) $ \ i -> 
            O.and $ for (A.range(a0,a1)) $ \ j ->
                O.not $ get r (i, j)
    in  farray (bounds r) $ \ (i,j) ->
            get t (i,j) O.&& ( normal A.! j )

nftonf r = 
    let t = symmetric_reflexive_transitive_closure r
        ((a0,_),(a1,_)) = bounds r
        normal = farray (a0,a1) $ \ i -> 
            O.and $ for (A.range(a0,a1)) $ \ j ->
                O.not $ get r (i, j)
    in  farray (bounds r) $ \ (i,j) ->
            O.and [ get t (i,j) , normal A.! i, normal A.! j]

-- * properties

null r = O.not $ O.or $ for (indices r) $ \ i -> get r i
full r = null $ complement r

isSubsetOf r s = null $ difference r s
equals r s = isSubsetOf r s O.&& isSubsetOf s r
disjoint r s = null $ intersection r s

diagonal r = O.and $ for (indices r) $ \ (i,j) ->
    O.constant (i == j) O.|| (O.not $ get r (i,j))

reflexive r = 
    let ((a0,_),(a1,_)) = bounds r
    in  O.and $ for (A.range(a0,a1)) $ \ i -> get r (i,i)
irreflexive r = 
    let ((a0,_),(a1,_)) = bounds r
    in  O.and $ for (A.range(a0,a1)) $ \ i -> O.not $ get r (i,i)
transitive r = isSubsetOf (times r r) r
symmetric r = isSubsetOf (inverse r) r
asymmetric r = null $ intersection r $ inverse r
antisymmetric r = diagonal $ intersection r $ inverse r
total r = full $ union r $ inverse r

sn r = irreflexive $ transitive_closure r

wn r = 
    let ((a0,b0),(a1,b1)) = bounds r
        b = bang r
    in  O.and $ for (A.range(a0,a1)) $ \ i -> 
            O.or $ for (A.range(b0,b1))  $ \ j -> get r (i,j)

un r = let b = bang r  in diagonal $ times (inverse b) b

unc r = 
    diagonal $ nftonf r

cr r = 
    let fore = reflexive_transitive_closure r 
        back = inverse fore
    in  isSubsetOf (times back fore) (times fore back)

wcr r = 
    let fore = reflexive_transitive_closure r 
        back = inverse fore
    in  isSubsetOf (times (inverse r) r) (times fore back)

-- * convenience functions
-- | for  building arrays
farray :: A.Ix i => (i,i) -> (i -> a) -> A.Array i a
farray bnd f = 
    A.array bnd $ map ( \ i -> (i, f i)) $ A.range bnd

for = flip map
