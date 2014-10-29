module Polynomial.ToDoc where

import Polynomial.Data
import Control.Lens
import Autolib.ToDoc

instance ToDoc v => ToDoc (Factor v) where
    toDoc f = case f ^. expo of
        1 -> toDoc $ f ^. var
        _ -> hcat [ toDoc $ f ^. var, text "^", toDoc $ f ^. expo ]

instance ToDoc v => ToDoc (Mono v) where
    toDoc m = hsep $ punctuate (text " *") $ map toDoc $ factors m

instance ToDoc v => ToDoc (Poly v) where
    toDoc p = case terms p of
        [] -> text "0"
        t : ts -> 
              let term (c,m) = case (c , nullMono m) of
                      _ | c < 0 -> hsep [ text "-", term (negate c, m) ]
                      ( _ , True) -> toDoc c
                      ( _ , False) | c == 1 -> toDoc m
                      ( _ , False) | c == -1 -> hsep [ text "-", toDoc m ]
                      _ -> hsep [ toDoc c, text "*", toDoc m ]
              in  hsep $ term t : map ( \ t @(c,m) -> if c < 0 then term t else text "+" <+>  term t) ts
