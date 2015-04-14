{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Goto.Type where

import RAM.Builtin

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Autolib.TES.Identifier

import Data.Typeable
import Autolib.Xml

type Register = Int
type Address = Int

data Statement
  = Inc Register | Dec Register
    | Assign Register Builtin [Register]
  | Stop | Goto Address | GotoZ Register Address

    deriving ( Eq, Ord, Typeable )

type Program = [ Statement ]

instance Size Program where 
    size = fromIntegral . length


$(derives [makeReader, makeToDoc] [''Statement])

instance Show Statement where show = render . toDoc



