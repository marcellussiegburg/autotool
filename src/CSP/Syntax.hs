{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module CSP.Syntax where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Data.Typeable
import qualified Data.Set as S

data Process a = Stop 
           | Pre a ( Process a )
           | Ext ( Process a ) ( Process a )
           | Int ( Process a ) ( Process a )
           | Seq ( Process a ) ( Process a )
           | Par [a] ( Process a ) ( Process a )
           | Fix ( Process a ) | Point
    deriving ( Eq, Ord )             
             
$(derives [ makeReader, makeToDoc ] [ ''Process ] )

instance Size ( Process a ) where
     size p = case p of
         Stop -> 1
         Pre x p -> succ $ size p
         Ext p q -> succ $ size p + size q
         Int p q -> succ $ size p + size q
         Seq p q -> succ $ size p + size q
         Par s p q -> succ $ size p + size q
         Fix p -> succ $ size p
         Point -> 1

alphabet :: Ord a => Process a -> S.Set a
alphabet p =  case p of
         Stop -> S.empty
         Pre x p -> S.insert x $ alphabet p
         Ext p q -> S.union ( alphabet p ) ( alphabet q )
         Int p q -> S.union ( alphabet p ) ( alphabet q )
         Seq p q -> S.union ( alphabet p ) ( alphabet q )
         Par s p q -> S.union ( alphabet p ) ( alphabet q )
         Fix p -> alphabet p
         Point -> S.empty

instance Hash a => Hash ( Process a ) where
     hash p = case p of
         Stop -> 17
         Pre x p -> hash (x, p)
         Ext p q -> hash (37 :: Int , p, q)
         Int p q -> hash (47 :: Int , p, q)
         Seq p q -> hash (57 :: Int , p, q)
         Par s p q -> hash (s, p, q)
         Fix p -> hash (67 :: Int, p)
         Point -> 27

example1 :: Process Char
example1 = Int ( Pre 'a' ( Pre 'b' Stop ))
              ( Pre 'a' ( Pre 'c' Stop ))

example2 :: Process Char
example2 = Ext ( Pre 'a' ( Pre 'b' Stop ))
              ( Pre 'a' ( Pre 'c' Stop ))

example3 :: Process Char
example3 = Par [] ( Pre 'a' ( Pre 'b' Stop ))
              ( Pre 'a' ( Pre 'c' Stop ))
