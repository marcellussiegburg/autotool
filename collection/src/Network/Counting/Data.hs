{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}

module Network.Counting.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Data.Typeable

newtype Wire = Wire Int deriving (Eq, Typeable, Num)

instance ToDoc Wire where toDoc (Wire i) = toDoc i
instance Reader Wire where reader = fmap Wire reader

data Balancer = B Wire Wire deriving Typeable

data Network = Network [ Balancer ] deriving Typeable

data Direction = Up | Down deriving Typeable

data State = State [(Balancer, Direction)]

$(derives [makeToDoc,makeReader] 
         [''Balancer,''Network,''Direction,''State])

instance Show Wire where show = render . toDoc
instance Show Network where show = render . toDoc
instance Show Direction where show = render . toDoc
instance Show State where show = render . toDoc

ex :: Network
ex = Network 
   [ B 1 2, B 3 4, B 1 4, B 2 3, B 1 2, B 3 4]