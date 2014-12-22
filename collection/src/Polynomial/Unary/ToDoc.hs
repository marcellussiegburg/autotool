{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

module Polynomial.Unary.ToDoc where

import Polynomial.Class 
import Polynomial.Patch
import Polynomial.Unary.Data

import Autolib.ToDoc

instance (Pattern c, Ring (Base c), ToDoc c, ToDoc e) 
         => ToDoc (P c e ) where
    toDoc (P (ces::[(c,e)])) = 
      if null ces then toDoc (inject zero :: c)
      else hsep $ punctuate (text " +") $ do
        (c,e) <- ces
        return $ toDoc c <+> text "*" <+> text "x" <+> text "^" <+> toDoc e

instance (Pattern c, Ring (Base c), ToDoc c) 
         => ToDoc (Poly c) where
    toDoc p = toDoc $ P $ terms p

instance (Pattern c, Ring (Base c), ToDoc c) 
         => Show (Poly c) where 
    show = render . toDoc
