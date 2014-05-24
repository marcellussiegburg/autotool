{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}

module Flow.Action where

import Autolib.TES.Identifier
import Autolib.Symbol


import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Hash
import GHC.Generics

-- | used in the alphabet for the automaton
data Action
    = Execute Identifier
    | Halt
  deriving ( Eq, Ord, Generic )

$(derives [makeToDoc,makeReader] [''Action])

instance Show Action where show = render . toDoc

instance Symbol Action -- ?
instance Size Action where size = const 1
instance Hashable Action 

