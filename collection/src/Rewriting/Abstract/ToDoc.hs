module Rewriting.Abstract.ToDoc where

import Rewriting.Abstract.Data

import Autolib.ToDoc 

instance ToDoc Prop where
    toDoc p = case p of
        Or ps -> hsep $ punctuate (text " ||") $ map toDoc ps
        And ps -> hsep $ punctuate (text " &&")$ map toDoc ps
        Not p -> text "not" <+> toDoc p
        PropParens p -> parens $ toDoc p
        Prop1 p1 x -> text (show p1) <+> parens ( toDoc x )
        Prop2 p2 x y -> text (show p2) <+> toDoc (x,y)

instance ToDoc Exp where
    -- this is a bit risky (it prints no parens on its own,
    -- so all parens must be present via ExpParens)
    toDoc x = case x of
        ExpParens x -> parens $ toDoc x
        Ref i -> toDoc i
        Op1 Complement x -> text "complement" <+> toDoc x
        Op1 o x -> toDoc x <+> case o of
            Inverse -> text "^-"
            Transitive_Closure -> text "^+"
            Transitive_Reflexive_Closure -> text "^*"
        Op2 o x y -> hsep 
            [ toDoc x
            , text $ case o of 
                  Product -> "."
                  Intersection -> "&"
                  Union -> "+"
                  Difference -> "-"
            , toDoc y ]
