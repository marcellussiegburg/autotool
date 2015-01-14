{-# language DisambiguateRecordFields #-}

module Up.ToDoc where

import Up.Data
import Autolib.ToDoc

instance ToDoc Name where
    toDoc (Name s) = text s

instance Show Name where show = render . toDoc

instance ToDoc Typ where
    toDoc t = case t of
        TUnit -> text "unit"
        TInt -> text "int"
        TBool -> text "bool"
        TFunc ts -> text "Func" 
           <> angles (map toDoc ts)

instance Show Typ where show = render . toDoc

angles = flowing_dutch (text "<", text ",", text ">")

instance ToDoc TypedName where
    toDoc (TypedName t n) = toDoc t <+> toDoc n

instance Show TypedName where show = render . toDoc

instance ToDoc Statement where
    toDoc s = case s of
        Halt -> text "halt" <+> text ";"
        Missing -> text "missing" <+> text ";"
        Declaration tn e -> 
           toDoc tn <+> text "=" <+> toDoc e <> text ";"
        Statement e -> toDoc e <> text ";"

instance Show Statement where show = render . toDoc

instance ToDoc Block where
    toDoc (Block ss) = braces' ( map toDoc ss )

instance Show Block where show = render . toDoc

instance ToDoc Exp where
    toDoc e = case e of
        ConstInteger i -> toDoc i
        Ref n -> toDoc n
        App f args -> toDoc f 
           <+> parens' ( map toDoc args )
        Program tns b -> 
           text  "function" <+>
           parens' (map toDoc tns) <+> toDoc b

instance Show Exp where show = render . toDoc

parens' = flowing_dutch (text "{", text ",", text "}" )
braces' = flowing_dutch (text "{", empty   , text "}" )




