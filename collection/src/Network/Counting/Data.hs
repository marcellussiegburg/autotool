{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}

module Network.Counting.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Autolib.Size
import Data.Typeable
import Data.Ix

newtype Wire = Wire Int 
    deriving (Eq, Ord, Ix, Typeable, Num)

instance ToDoc Wire where toDoc (Wire i) = toDoc i
instance Reader Wire where reader = fmap Wire reader
instance Hash Wire where hash (Wire i) = hash i

type Balancer = (Wire, Wire)

data Network = Network [ Balancer ] 
     deriving (Eq, Typeable)

$(derives [makeToDoc,makeReader] [''Network])

instance Show Wire where show = render . toDoc
instance Show Network where show = render . toDoc
instance Size Network where
    size (Network bs) = length bs
instance Hash Network where hash (Network bs) = hash bs

ex :: Network
ex = Network 
   [ (1, 2 ), (3, 4 ), (1, 4 )
   , (2, 3 ), (1, 2 ), (3, 4 )
   ]