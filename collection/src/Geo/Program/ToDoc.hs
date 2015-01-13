{-# language TemplateHaskell #-}

module Geo.Program.ToDoc where

import Geo.Program.AST

import Autolib.ToDoc

instance ToDoc Op where
    toDoc op = case op of
      Add -> text "+" ; Subtract -> text "-"
      Multiply -> text "*" ; Divide -> text "/"

instance (ToDoc v) => ToDoc (Exp v) where
    toDoc e = case e of
        Const i -> toDoc i
        Ref n -> toDoc n
        Oper x op y -> fsep [ toDoc x, toDoc op, toDoc y ]
        Parens e -> parens $ toDoc e
        Apply f args ->
            toDoc f <+> dutch_tuple ( map toDoc args )
        Block stmts val ->
          braces $ align $ vcat $ map (\ d -> d <> text ";" ) 
                $ map toDoc stmts
            ++ [ text "return" <+> toDoc val ]

derives [makeToDoc] [''Type]

instance (ToDoc v) => ToDoc (Statement v) where
    toDoc (Decl f (Just args) (Just b)) =
      toDoc f <+> dutch_tuple (map toDoc args) <+> toDoc b
    toDoc (Decl f Nothing (Just b)) =
      toDoc f <+> text "=" <+> toDoc b
    toDoc (Decl f Nothing Nothing) =
      toDoc f 
    toDoc (Emit k e) = toDoc k <+> toDoc e

instance ToDoc v => ToDoc (Typed v) where
    toDoc (Typed t v) = toDoc t <+> toDoc v

derives [makeToDoc] [''Kind]
