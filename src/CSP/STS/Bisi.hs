module CSP.STS.Bisi where

import CSP.STS.Type
import CSP.STS.Tau
import CSP.STS.Roll

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import qualified Autolib.Relation as R
import qualified Data.Set as S
import Autolib.Set ( cross )
import Control.Monad ( guard, void, forM )

checks = do
    let n = 5 :: Int
    s <- roll [1..n ] "ab" 
    t <- roll [1..n ] "ab"  
    case bisi (s,t) of
        Nothing -> checks
        Just r -> do
            print s
            print t
            print r
            return r

s = STS { start = 3 , alphabet = S.fromList [ 'a' , 'b' ]
    , visible = [ ( 2 , 'b' , 4 ) , ( 1 , 'b' , 4 ) , ( 4 , 'a' , 2 )
                , ( 4 , 'a' , 1 )
                ]
    , hidden = [ ( 3 , 4 ) , ( 2 , 1 ) ]
    }

check_bisi (s,t) r = do
    let miss = S.difference ( states s ) ( domain r )
    when ( not $ S.null miss ) $ reject 
         $ text "Diese Zustände aus S sind nicht im Vorbereich von R enthalten:"
         </> toDoc miss
    let mist = S.difference ( states t ) ( codomain r )
    when ( not $ S.null mist ) $ reject 
         $ text "Diese Zustände aus T sind nicht im Nachbereich von R enthalten:"
         </> toDoc mist
    when ( not $ R.holds r (start s) (start t) ) $ reject
         $ text "Es gilt nicht  R(start s, start t)"
    forM ( visible s ) $ \ (p,a,p') -> 
        forM ( S.toList $ R.images r p ) $ \ q -> do
            let imgs = S.fromList $ images t q a
                sims = R.images r p'
                both = S.intersection imgs sims 
            inform $ text "Transition in S:" <+> toDoc (p,a,p') </> vcat 
                          [ text "Simulation in R:" <+> toDoc (p,q)
                          , text "durch Transition in T erreichbar:" </> toDoc imgs
                          , text "durch Simulation erreichbar:" </> toDoc sims
                          , text "gemeinsam erreichbar:" </> toDoc both
                          ]
            when  ( S.null both ) $ reject $ text "nicht zusammenführbar."


bisi (s,t) = do
    let r = last $ bisis (s,t)
    guard $ and [ R.holds r ( start s ) ( start t )
            , states s == domain r
            , states t == codomain r
            ]
    return r

domain = S.fromList . map fst . R.pairs 
codomain = domain . R.inverse

bisis ::  ( Ord s1, Ord s2, Ord t )
     => (STS s1 t , STS s2 t ) 
     -> [ R.Type s1 s2 ]
bisis (s,t) =
    let handle r = r : 
            let r' = refine (s,t) r
            in  if r == r' then [] else handle r'
    in  handle $ coarse (s,t)

coarse (s,t) = R.make $ S.toList $ cross ( states s ) ( states t )

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


