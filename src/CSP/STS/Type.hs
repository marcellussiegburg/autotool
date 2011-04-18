-- | state transitions systems

{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module CSP.STS.Type where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Hash

import Autolib.Set
import qualified Data.Set as S
import Data.Typeable

data Ord t => STS s t = STS { start :: s
                   , alphabet :: Set t
                   , visible :: [ (s, t, s) ]  
                   , hidden :: [ (s, s) ]
                   }  deriving ( Eq, Ord, Typeable )
               
instance ( Hash s, Hash t, Ord t ) => Hash ( STS s t ) where
    hash s = hash ( visible s, hidden s )

states :: (Ord t, Ord s ) => STS s t -> Set s
states s = S.fromList 
     $ start s
     :  do (p,t,q) <- visible s ; [p, q]
     ++ do (p,q)   <- hidden  s ; [p, q]    

$(derives [ makeReader, makeToDoc ] [ ''STS ] )