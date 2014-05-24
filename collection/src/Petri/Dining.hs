{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

module Petri.Dining where

import Petri.Type hiding ( Transition )
import Petri.Dot
import Petri.Step

import Autolib.Dot.Dot

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Autolib.Size
import Data.Typeable
import GHC.Generics

import qualified Data.Set as S
import qualified Data.Map as M

newtype Philo = Philo Int 
    deriving ( Eq, Ord, Num, ToDoc, Reader, Hashable, Typeable, Generic )
newtype Gabel = Gabel Int 
    deriving ( Eq, Ord, Num, ToDoc, Reader, Hashable, Typeable, Generic )

data Stelle = P Philo Int | G Gabel deriving ( Eq, Ord, Typeable, Generic )
instance Hashable Stelle

data Transition = Tak Philo Gabel | Drp Philo deriving ( Eq, Ord, Typeable, Generic )
instance Hashable Transition  

instance Size Transition where size _ = 1

$(derives [makeReader, makeToDoc] [''Stelle, ''Transition ])

instance Show Stelle where show = render . toDoc
instance Show Transition where show = render . toDoc
                          

make :: ( Ord s, Ord t )
     => [s ]
     -> [ Connection s t ] -> Net s t
make st cs = 
    let ts = S.fromList $ do ( vor, t, nach ) <- cs ; return t
        ps = S.fromList $ do ( vor, t, nach ) <- cs ; vor ++ nach
    in  Net { places = ps , transitions = ts , connections = cs     
            , capacity = Unbounded 
            , start = State $ M.fromList $ do
                  p <- S.toList ps 
                  return ( p, if p `elem` st then 1 else 0 )
            }          

diner n = make ( do i <- [ 1 .. n ] ; [ P ( Philo i ) 0, G ( Gabel i ) ] )
   $  do i <- [ 1 .. n ] ; let p = Philo i  
         let left = Gabel i ; right = Gabel $ succ (i `mod` n) 
         [   ( [ P p 0 , G right ] , Tak p right, [ P p 1 ] ) 
           , ( [ P p 1 , G left ] , Tak p left, [ P p 2 ] ) 
           , ( [ P p 2 ], Drp p, [ P p 0,  G right, G left ] )
           ]
           


    
                   
