{-# LANGUAGE TemplateHaskell #-}

module While.Property where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

import RAM.Builtin

data Property = Builtins [ Builtin ]
              | No_While
              | No_Loop
              | No_IfZ
              | Max_Register Int
     deriving ( Typeable , Eq )

$(derives [makeReader, makeToDoc] [''Property])

instance Show Property where show = render . toDoc

example :: [ Property ]
example = [ Builtins [ ]
          , No_While
	  ]

