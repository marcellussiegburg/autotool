{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveGeneric #-}

module Network.Counting.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Autolib.Size
import Data.Typeable
import Data.Ix
import GHC.Generics

newtype Wire = Wire Int 
    deriving (Eq, Ord, Ix, Typeable, Enum, Num, Generic)

instance ToDoc Wire where toDoc (Wire i) = toDoc i
instance Reader Wire where reader = fmap Wire reader
instance Hashable Wire 

type Balancer = (Wire, Wire)

data Network = Network [ Balancer ] 
     deriving (Eq, Typeable, Generic)

wires (Network bs) = do (top,bot) <- bs ; [ top, bot ]

$(derives [makeToDoc,makeReader] [''Network])

instance Show Wire where show = render . toDoc
instance Show Network where show = render . toDoc
instance Size Network where
    size (Network bs) = length bs
instance Hashable Network 

ex :: Network
ex = Network 
   [ (1, 2 ), (3, 4 ), (1, 4 )
   , (2, 3 ), (1, 2 ), (3, 4 )
   ]
