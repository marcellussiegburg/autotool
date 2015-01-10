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
          braces $ align $ vcat $ map (\ d -> d <> text ";" ) 
                $ map toDoc decls
            ++ [ text "return" <+> toDoc val ]

derives [makeToDoc] [''Type]

instance (ToDoc v) => ToDoc (Decl v) where
    toDoc (Decl f (Just args) (Just b)) =
      toDoc f <+> dutch_tuple (map toDoc args) <+> toDoc b
    toDoc (Decl f Nothing (Just b)) =
      toDoc f <+> text "=" <+> toDoc b
    toDoc (Decl f Nothing Nothing) =
      toDoc f 

instance ToDoc v => ToDoc (Typed v) where
    toDoc (Typed t v) = toDoc t <+> toDoc v
