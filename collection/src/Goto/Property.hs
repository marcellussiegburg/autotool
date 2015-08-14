{-# LANGUAGE TemplateHaskell #-}

module Goto.Property where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

import RAM.Builtin

data Property = Builtins [ Builtin ]
     deriving ( Typeable , Eq )

$(derives [makeReader, makeToDoc] [''Property])

instance Show Property where show = render . toDoc

example :: [ Property ]
example = [ Builtins [ ]
	  ]

