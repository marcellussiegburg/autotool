module CSP.STS.Bisi where

import CSP.STS.Type
import CSP.STS.Tau
import CSP.STS.Roll

import qualified Autolib.Relation as R
import qualified Data.Set as S
import Autolib.Set ( cross )
import Control.Monad ( guard )


check = do
    let n = 4 :: Int
    s <- roll [1..n ] "ab" 
    t <- roll [1..n ] "ab"  
    print $ bisi (s,t)

bisi ::  ( Ord s1, Ord s2, Ord t )
     => (STS s1 t , STS s2 t ) 
     -> R.Type s1 s2
bisi (s,t) =
    let r = R.make $ S.toList $ cross ( states s ) ( states t )
        handle r = 
            let r' = refine (s,t) r
            in  if r == r' then r else handle r'
    in  handle r    


refine ::  ( Ord s1, Ord s2, Ord t )
     => (STS s1 t , STS s2 t ) 
     -> R.Type s1 s2 
     -> R.Type s1 s2
refine (s, t) r = R.make $ do
    (p,q) <- R.pairs r
    guard $ from s t r
    guard $ from t s $ R.inverse r
    return ( p, q)

from :: ( Ord s1, Ord s2, Ord t )
     => STS s1 t -> STS s2 t -> R.Type s1 s2 -> Bool
from s t r = and $ do
    (p, q) <- R.pairs r
    a <- S.toList $ alphabet s
    p' <- images s p a
    return $ or $ do
            q' <- images t q a
            return $ R.holds r p' q'

images s p a = do
    (from, b, to) <- visible s
    guard $ from == p && b == a
    return to


