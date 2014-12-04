module Polynomial.Unary.ToDoc where

import Polynomial.Unary
import Autolib.ToDoc

instance (ToDoc c, ToDoc e) => ToDoc (P c e ) where
    toDoc (P ces) = hsep $ punctuate (text "+") $ do
        (c,e) <- ces
        return $ toDoc c <+> text "*" <+> text "x" <+> text "^" <+> toDoc e
