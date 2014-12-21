{-# language TemplateHaskell #-}

module Geo.Program.ToDoc where

import Geo.Program.AST

import Autolib.ToDoc

instance (ToDoc v) => ToDoc (Exp v) where
    toDoc e = case e of
        Ref n -> toDoc n
        Apply f args ->
            toDoc f <+> dutch_tuple ( map toDoc args )
        Block decls val ->
          dutch (text "{" , text ";", text "}")
            $ map toDoc decls
            ++ [ text "return" <+> toDoc val ]

derives [makeToDoc] [''Type]

instance (ToDoc v) => ToDoc (Decl v) where
    toDoc (Decl f margs b) =
        toDoc f 
        <+> ( case margs of
           Nothing -> text "="
           Just args -> dutch_tuple $ map toDoc args )
        <+> toDoc b

instance ToDoc v => ToDoc (Typed v) where
    toDoc (Typed t v) = toDoc t <+> toDoc v
