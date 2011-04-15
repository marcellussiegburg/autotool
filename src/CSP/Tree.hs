module CSP.Tree ( toTree, display, peng ) where

import CSP.Syntax
import Tree
import Autolib.Dot.Dotty

instance Show a => ToTree ( Process a ) where
    toTree p = case p of
        Stop -> Node "Stop " []
        Pre x p -> Node ( "Pre " ++ show x ) [ toTree p ]
        Ext p q -> Node "Ext" $ map toTree [ p, q ]
        Int p q -> Node "Int" $ map toTree [ p, q ]
        Seq p q -> Node "Seq" $ map toTree [ p, q ]
        Par s p q -> 
            Node ( "Par " ++ show s ) $ map toTree [ p, q ]
        Fix p -> Node "Fix" [ toTree p ]
        Point -> Node "Point" [] 
        
