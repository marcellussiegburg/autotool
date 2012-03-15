-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Graph.PartialKTree.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = 
     Config { nodes :: Int
	    , width :: Int -- of clique
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

instance Show Config where show = render . toDoc
                           
example :: Config
example = Config 
	{ nodes = 10
	, width = 2
	}

