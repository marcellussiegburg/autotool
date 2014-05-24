{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# language DeriveGeneric #-}

module Tree.Class where

import Tree.Dot

import Data.Tree
import Autolib.Dot

import qualified Autolib.Dot.Node

import Autolib.Hash
import Autolib.ToDoc

import GHC.Generics

class ToTree baum where
      -- | easy interface:
      toTree :: baum -> Tree String
      -- | detailed interface (if you want to fine-tune the layout)
      -- the node property ident will be set by the program
      toTreeNode :: baum -> Tree Autolib.Dot.Node.Type
      toTreeNode = fmap default_node . toTree

instance ToDot ( Tree Autolib.Dot.Node.Type ) where
    toDotProgram t = Dot
    toDotOptions t = unwords [ "-Gordering=out", "-Gnodesep=0" ]
    toDot t = make t 

instance ( ToTree baum ) => ToDot baum  where
    toDotProgram = toDotProgram . toTreeNode
    toDotOptions = toDotOptions . toTreeNode
    toDot        = toDot        . toTreeNode



instance Hash a => Hashable ( Tree a ) where  
    hashWithSalt s ( Node f args ) = hashWithSalt s ( f, args )

instance ( Eq baum, ToTree baum ) => Hashable baum  where    
    hashWithSalt s = hashWithSalt s . toTree

mirror :: Tree a -> Tree a
mirror ( Node f args ) = Node f ( map mirror args )

form :: ( ToTree baum ) 
     => baum -> Doc
form = vcat . map text . lines . drawTree . mirror 
     . fmap ( render . toDoc )
     . toTree



