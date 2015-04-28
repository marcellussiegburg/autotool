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

example =
  [ GotoZ 1 4 , Inc 0 , Dec 1 , Goto 0 , Stop ]

instance Size Program where 
    size = fromIntegral . length

free_register :: Program -> Register
free_register p = maximum $ 0 : map succ ( do
  s <- p
  case s of
    Inc r -> [r] ; Dec r -> [r]
    Assign res fin args -> res : args
    GotoZ r _ -> [r]
    _ -> [] )

$(derives [makeReader, makeToDoc] [''Statement])

instance Show Statement where show = render . toDoc



