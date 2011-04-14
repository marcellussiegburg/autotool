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
     size p = length $ subs p

alphabet :: Ord a => Process a -> S.Set a
alphabet p = S.fromList $ do Pre x _ <- subs p ; return x

subs :: Process a -> [ Process a ]
subs p = map snd $ splits p

splits :: Process a 
       -> [ (Process a -> Process a, Process a) ]
splits p = ( id, p ) : case p of       
    Stop -> [] 
    Pre x p -> splits1  ( Pre x ) p
    Ext p q -> splits2 Ext p q
    Int p q -> splits2 Int p q
    Seq p q -> splits2 Seq p q
    Par s p q -> splits2 ( Par s ) p q
    Fix p -> splits1 Fix p
    Point -> []

splits1 c p = 
    map ( \ (f,a) -> (c . f, a)) $ splits p
splits2 c p q = 
       splits1 ( \ p' -> c p' q ) p
    ++ splits1 ( \ q' -> c p q' ) q


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
