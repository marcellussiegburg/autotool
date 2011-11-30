module CSP.STS.Roll where

import CSP.STS.Type
import CSP.STS.Dot

import Autolib.Util.Zufall
import Autolib.Set
import qualified Autolib.Relation as R

import qualified Data.Set as S
import Control.Monad ( forM )
import Data.List ( nub )

mutate :: ( Ord t, Ord s )
     => Int
     -> STS s t
     -> IO ( STS s t )
mutate d s = do
    let ss = S.toList $ states s ; ts = S.toList $ alphabet s
    vis <- permutation $ visible s
    vis' <- forM [ 1 .. d ] $ \ k -> do
        p <- eins ss ; t <- eins ts ; q <- eins ss
        return ( p, t, q )
    return $ s { visible = nub $ vis' ++ drop d vis }

roll :: ( Ord t, Eq s )
     => [s] -> [t] 
     -> Int 
     -> Int
     -> IO ( STS s t )
roll ss ts num_visible num_hidden = do
    vis <- forM [ 1 .. num_visible ] $ \ k -> do
        p <- eins ss ; t <- eins ts ; q <- eins ss
        return ( p, t, q )
    hid <- forM [ 1 .. num_hidden  ] $ \ k -> do
        p <- eins ss ; q <- eins ss
        return ( p, q )
    st <- eins ss
    return $ STS 
           { alphabet = S.fromList ts
           , start = st
           , visible = nub vis
           , hidden = nub hid 
           }
                  
roll_reachable ss ts num_visible num_hidden = do
    s <- roll ss ts num_visible num_hidden
    mp <- find_start s    
    case mp of 
        Nothing -> 
            roll_reachable ss ts num_visible num_hidden 
        Just p -> return $ s { start = p }    

    
-- | return ( Just p) for some state p     
-- than can reach all states.    
-- if there is no such state, return Nothing    
find_start s = do 
    let sts = states s
        reach = R.reflex_trans
            $ R.make $ do (p,a,q) <- visible s ; return(p,q)
                    ++ do (p,  q) <- hidden  s ; return(p,q)
        candidates = 
            S.filter ( \ p -> R.images reach p == sts ) sts
    if S.null candidates then return Nothing
        else fmap Just $ eins $ S.toList candidates
        