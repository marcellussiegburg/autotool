{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Code.Huffman.LR where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Symbol
import Autolib.Hash
import Autolib.Size

import Code.Type

import Data.Typeable
import GHC.Generics

data LR = L | R 
     deriving ( Eq, Ord, Enum, Bounded, Typeable, Generic )

$(derives [makeReader, makeToDoc] [''LR])
instance Show LR where show = render . toDoc
                          

instance Symbol LR 
instance Size LR where size _ = 1
instance Hashable LR 

data Ord a => Letter a = Letter 
	      { weight :: Int
	      , codes  :: Code a LR
	      }
     
$(derives [makeReader, makeToDoc] [''Letter])







