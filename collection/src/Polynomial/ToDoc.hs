{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

module Polynomial.ToDoc where

import qualified Prelude  
import Prelude
  hiding ( Num (..), (^), sum, map, Integer, Rational, fromInteger)
import Polynomial.Class

import Polynomial.Data
import Control.Lens
import Autolib.ToDoc

instance ToDoc v => ToDoc (Factor v) where
    toDoc f = case f ^. expo of
        1 -> toDoc $ f ^. var
        _ -> hsep [ toDoc $ f ^. var, text "^", toDoc $ f ^. expo ]

instance ToDoc v => ToDoc (Mono v) where
    toDoc m = hsep $ punctuate (text " *")
              $ Prelude.map toDoc $ factors m

instance (Ring r, Ord r, ToDoc r, ToDoc v, Ord v) 
         => ToDoc (Poly r v) where
    toDoc p = case terms p of
        [] -> text "0"
        t : ts -> 
              let term (c,m) = case (c , nullMono m) of
                      _ | negative c -> hsep [ text "-", term (negate c, m) ]
                      ( _ , True) -> toDoc c
                      ( _ , False) | c == one -> toDoc m
                      ( _ , False) | c == negate one -> hsep [ text "-", toDoc m ]
                      _ -> hsep [ toDoc c, text "*", toDoc m ]
              in  hsep $ term t : Prelude.map ( \ t @(c,m) -> if negative c then term t else text "+" <+>  term t) ts

instance ToDoc (Poly r v) => Show (Poly r v) where
    show = render . toDoc 
