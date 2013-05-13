{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module CSP.Step where

import CSP.Syntax
import CSP.STS

import Autolib.Reader
import Autolib.ToDoc

import qualified Data.Set as S
import Data.List ( nub )
import Control.Monad ( guard )
import Data.Typeable

data Step a = Tau | Real a deriving ( Eq, Ord, Typeable )

$(derives [makeToDoc, makeReader] [''Step])

instance ToDoc a => Show (Step a) where show = render . toDoc

sts :: Ord a => Process a -> STS ( Process a ) a
sts p = 
    let handle done vis hid todo = case S.minView todo of
            Nothing -> STS { start = p
                           , CSP.STS.alphabet = 
                               S.fromList $ do 
                                 (p,t,q) <- vis ; return t
                           , visible = vis, hidden = hid
                           }                          
            Just ( t, odo ) -> 
                let done' = S.insert t done 
                    rs = real t
                    ts = taus t
                    next = S.union odo $ S.fromList 
                         $ map snd rs ++ ts
                in  handle done'
                       (map ( \ (a,r) ->(t,a,r)) rs ++ vis) 
                       (map ( \ r -> (t,r) ) ts ++ hid )
                       (S.difference next done')
    in  handle S.empty [] [] $ S.singleton p



successors :: Eq a => Process a -> [ ( Step a, Process a ) ]
successors p = 
      do ( t, q ) <- real p ; return ( Real t, q )
   ++ do q <- tau p ; return ( Tau, q )     

real :: Eq a => Process a -> [ (a, Process a) ]
real p = nub $ case p of
    Stop -> []
    Pre x p -> [ (x, p) ]
    Ext p q -> real p ++ real q
    Int p q -> [] -- man muß erst den tau-Schritt machen
    Star p -> []  -- man muß erst den tau-Schritt machen
    Seq p q -> case p of
        Stop -> real q
        _    -> do (a,p') <- real p ; return ( a, Seq p' q )
    Par s p q -> 
         do (a, p') <- real p
            guard $ not $ elem a s
            return (a, Par s p' q )
      ++ do (b, q') <- real q
            guard $ not $ elem b s
            return (b, Par s p q' )  
      ++ do
            (a, p') <- real p
            (b, q') <- real q
            guard $ (a `elem` s) && ( a == b )
            return ( a, Par s p' q' )
    Fix p -> real ( subst p ( Fix p ) )
    Point -> error "real: Point outside Fix"

-- | reachable (in one or more steps)
taus :: Ord a => Process a -> [ Process a ]
taus p = 
    let handle done todo = case S.minView todo of
            Nothing -> S.toList done
            Just (t, odo) -> 
                let done' = S.insert t done
                    todo' = S.union odo $ S.fromList $ tau t
                in  handle done' $ S.difference todo' done' 
    in  handle S.empty $ S.fromList $ tau p                
           

tau :: Eq a => Process a -> [ Process a ]
tau p = nub $ case p of
    Stop -> []
    Pre x p -> [ ]
    Ext p q -> do p' <- tau p ; return $ Ext p' q
           ++  do q' <- tau q ; return $ Ext p q' 
    Int p q -> [p, q]
    Seq p q -> case p of
        Stop -> return q
        _    -> do p' <- tau p ; return ( Seq p' q )
    Par s Stop Stop -> return Stop
    Par s p q -> do p' <- tau p ; return $ Par s p' q
             ++  do q' <- tau q ; return $ Par s p q' 
    Star p -> [ Stop, Seq p (Star p) ]
    Fix p -> tau ( subst p ( Fix p ) )
    Point -> error "tau: Point outside Fix"

subst r s = case r of
    Stop -> Stop
    Pre x p -> Pre x ( subst p s )
    Ext p q -> Ext (subst p s) ( subst q s )
    Int p q -> Int (subst p s) ( subst q s )
    Seq p q -> Seq (subst p s) ( subst q s )
    Par a p q -> Par a (subst p s) ( subst q s )
    Fix f -> Fix f
    Point -> s
    
-- testing -------------------------------------------
    
p = Fix (Pre 'b' 
           (Int (Par [ 'a' ] (Pre 'b' Point) (Pre 'a' Stop))
                (Pre 'a' Point)))
    
[('b', q)] = real p    

[ q1, q2 ] = taus q

[ ('b', r) ] = real q2

