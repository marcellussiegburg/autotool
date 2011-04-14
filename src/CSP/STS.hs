-- | state transitions systems

{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module CSP.STS where

import Autolib.Reader
import Autolib.ToDoc

import qualified Data.Set as S

data STS s t = STS { start :: s
                   , visible :: [ (s, t, s) ]  
                   , hidden :: [ (s, s) ]
                   }  
               
states :: Ord s => STS s t -> S.Set s
states s = S.fromList 
     $ start s
     :  do (p,t,q) <- visible s ; [p, q]
     ++ do (p,q)   <- hidden  s ; [p, q]    

$(derives [ makeReader, makeToDoc ] [ ''STS ] )