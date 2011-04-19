module CSP.STS.Bisi.Refine where

import CSP.STS.Type

import qualified Autolib.Relation as R
import qualified Data.Set as S
import Autolib.Set ( cross )
import Control.Monad ( guard, void, forM, MonadPlus )

bisi :: MonadPlus m 
     => ( Ord s1, Ord s2, Ord t )
     => (STS s1 t , STS s2 t ) 
     -> m ( R.Type s1 s2  )
bisi (s,t) = do
    let r = last $ bisis (s,t)
    guard $ and [ R.holds r ( start s ) ( start t )
            , states s == domain r
            , states t == codomain r
            ]
    return r

bisis ::  ( Ord s1, Ord s2, Ord t )
     => (STS s1 t , STS s2 t ) 
     -> [ R.Type s1 s2 ]
bisis (s,t) =
    let handle r = r : 
            let r' = refine (s,t) r
            in  if r == r' then [] else handle r'
    in  handle $ coarse (s,t)

coarse (s,t) = R.make $ S.toList 
             $ cross ( states s ) ( states t )

refine ::  ( Ord s1, Ord s2, Ord t )
     => (STS s1 t , STS s2 t ) 
     -> R.Type s1 s2 
     -> R.Type s1 s2
refine (s, t) r = R.make $ do
    (p,q) <- R.pairs r
    guard $ from s t r (p,q)
    guard $ from t s ( R.inverse r ) ( q,p )
    return ( p, q)

from :: ( Ord s1, Ord s2, Ord t )
     => STS s1 t -> STS s2 t -> R.Type s1 s2 
     -> (s1, s2)
     -> Bool
from s t r (p,q) = and $ do
    a <- S.toList $ alphabet s
    p' <- images s p a
    return $ or $ do
            q' <- images t q a
            return $ R.holds r p' q'

images s p a = do
    (from, b, to) <- visible s
    guard $ from == p && b == a
    return to


domain :: ( Ord a, Ord b ) => R.Type a b -> S.Set a
domain = S.fromList . map fst . R.pairs 

codomain :: ( Ord a, Ord b ) => R.Type a b -> S.Set b
codomain = domain . R.inverse
