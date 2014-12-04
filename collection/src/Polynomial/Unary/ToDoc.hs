module Polynomial.Unary.ToDoc where

import Polynomial.Unary.Data

import Autolib.ToDoc

instance (ToDoc c, ToDoc e) => ToDoc (P c e ) where
    toDoc (P ces) = hsep $ punctuate (text "+") $ do
        (c,e) <- ces
        return $ toDoc c <+> text "*" <+> text "x" <+> text "^" <+> toDoc e

instance (ToDoc c) => ToDoc (Poly c) where
    toDoc p = toDoc $ P $ terms p

instance ToDoc c => Show (Poly c) where 
    show = render . toDoc
