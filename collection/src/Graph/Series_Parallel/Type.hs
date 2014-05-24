{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Graph.Series_Parallel.Type where

import Autolib.Graph.Type
import Autolib.Graph.Basic
import Autolib.Reader
import Autolib.ToDoc
import Data.Autolib.Transport
import Autolib.Hash
import Data.Typeable
import GHC.Generics

data GraphC a => STGraph a = STGraph
               { source :: a
               , target :: a
               , contents :: Graph a
               }
    deriving ( Typeable 
	     -- , Ord, Eq 
	     )

$(derives [makeReader, makeToDoc] [''STGraph])

instance ( GraphC a ) => Hashable ( STGraph a )  where
    hashWithSalt s g = hashWithSalt s ( source g, target g, contents g )
instance GraphC a => Show (STGraph a) where 
    show = render . toDoc
                           
example :: GraphC Int => STGraph Int
example = STGraph { source = 1, target = 3
                  , contents = circle [ 1 .. 5 ]
                  }

data Threeway a b = This a | That b | Both Int
   deriving ( Typeable , Ord, Eq, Generic )

$(derives [makeReader, makeToDoc, makeToTransport] [''Threeway])

instance (ToDoc a, ToDoc b) => Show (Threeway a b) where show = render . toDoc
                           
instance ( Hash a, Hash b ) => Hashable ( Threeway a b ) 


