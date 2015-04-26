{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module While.Type where

import RAM.Builtin

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Autolib.Xml

type Register = Int

data Program = Inc Register
	 | Dec Register
         | Assign Register Builtin [Register] 
         | Skip
         | Seq Program Program
         | IfZ Register Program Program
	 | Loop Register Program
	 | While Register Program
    deriving ( Eq, Ord, Typeable )

example :: Program
example = While 1 (Seq (Dec 1) (Inc 0))

instance Size Program where
  size p = case p of
    Seq p1 p2 -> succ $ size p1 + size p2
    IfZ r p1 p2 -> succ $ size p1 + size p2
    While r p1 -> succ $ size p1
    Loop r p1 -> succ $ size p1
    _ -> 1

subprograms :: Program -> [Program ]
subprograms p = p : case p of
  Seq p1 p2 -> subprograms p1 ++ subprograms p2
  IfZ _ p1 p2 -> subprograms p1 ++ subprograms p2
  While _ p1 ->  subprograms p1
  Loop _ p1 ->  subprograms p1
  _ -> []

$(derives [makeReader, makeToDoc] [''Program])

instance Show Program where show = render . toDoc

shorten d p = if d < 0 then text "..." else case p of
  Seq p1 p2 -> text "Seq" <+> psd d p1 <+> psd d p2
  IfZ r p1 p2 ->
    text "IfZ" <+> toDoc r <+> psd d p1 <+> psd d p2
  While r p1 -> text "While" <+> psd d p1
  Loop r p1 -> text "Loop" <+> psd d p1
  _ -> toDoc p

psd d p = parens $ shorten (d-1) p
