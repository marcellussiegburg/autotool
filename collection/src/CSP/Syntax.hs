{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}

module CSP.Syntax where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Data.Typeable
import GHC.Generics
import qualified Data.Set as S

data Process a = Stop 
           | Pre a ( Process a )
           | Ext ( Process a ) ( Process a )
           | Int ( Process a ) ( Process a )
           | Seq ( Process a ) ( Process a )
           | Par [a] ( Process a ) ( Process a )
           | Fix ( Process a ) | Point
           | Star ( Process a )
           | Undefined
    deriving ( Eq, Ord, Generic ) 
             
$(derives [ makeReader, makeToDoc ] [ ''Process ] )

instance ToDoc a => Show (Process a) where show = render . toDoc
                          
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
    Star p -> splits1 Star p
    Undefined -> []

splits1 c p = 
    map ( \ (f,a) -> (c . f, a)) $ splits p

splits2 c p q = 
       splits1 ( \ p' -> c p' q ) p
    ++ splits1 ( \ q' -> c p q' ) q


instance Hash a => Hashable ( Process a )

example1 :: Process Char
example1 = Int ( Pre 'a' ( Pre 'b' Stop ))
              ( Pre 'a' ( Pre 'c' Stop ))

example2 :: Process Char
example2 = Ext ( Pre 'a' ( Pre 'b' Stop ))
              ( Pre 'a' ( Pre 'c' Stop ))

example3 :: Process Char
example3 = Par [] ( Pre 'a' ( Pre 'b' Stop ))
              ( Pre 'a' ( Pre 'c' Stop ))
