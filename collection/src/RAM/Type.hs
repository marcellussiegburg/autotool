{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RAM.Type where

import RAM.Builtin

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Autolib.TES.Identifier

import Data.Typeable
import Autolib.Xml

type Var = Identifier

instance Container Identifier String where
    label _ = "Autolib.TES.Identifier"
    pack = show
    unpack = read

data Statement = Inc Var
	 | Dec Var
	 | Loop Var Program
	 | While Var Program
         | Assign Var Builtin [Var] -- ^ recommended

	 | Builtin { name :: Builtin, res :: Var, args :: [ Var ] } -- ^ deprecated
    deriving ( Eq, Ord, Typeable )

type Program = [ Statement ]

instance Size Program where 
    size = sum . map size
instance Size Statement where
    size ( Loop v p ) = succ $ size p
    size ( While v p ) = succ $ size p
    size _ = 1

flatten :: Program -> Program
flatten ps = do
    p <- ps
    p : case p of Loop v q -> flatten q
		  While v q -> flatten q
		  _ -> []

$(derives [makeReader, makeToDoc] [''Statement])



