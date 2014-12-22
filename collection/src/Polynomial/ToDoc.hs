{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

module Polynomial.ToDoc where

import Polynomial.Data
import Control.Lens
import Autolib.ToDoc

instance ToDoc v => ToDoc (Factor v) where
    toDoc f = case f ^. expo of
        1 -> toDoc $ f ^. var
        _ -> hsep [ toDoc $ f ^. var, text "^", toDoc $ f ^. expo ]

instance ToDoc v => ToDoc (Mono v) where
    toDoc m = hsep $ punctuate (text " *") $ do
        (v,e) <- factors m ; return $ toDoc $ Factor { _var = v, _expo = e }

instance (Num r, Ord r, ToDoc r, ToDoc v) 
         => ToDoc (Poly r v) where
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

instance ToDoc (Poly r v) => Show (Poly r v) where
    show = render . toDoc 
