module Fourier_Motzkin.ToDoc where

import Fourier_Motzkin.Data

import Autolib.ToDoc

import Data.Ratio
import qualified Data.Map as M

instance ToDoc v => ToDoc (Linear v) where
    toDoc (Linear m) = hsep $ do
        (mv,co) <- M.toList m
        let sign = if co>0 then text "+ " else empty
            co_times_var = case mv of
                Nothing -> toDoc co
                Just v -> (if co /= 1 then toDoc co <+> text "*" else empty ) <+> toDoc v
        return $ sign <> co_times_var

instance ToDoc v => ToDoc (Atom v) where
    toDoc a = text "0" <+> (if strict a then text "<" else text "<=" ) <+> toDoc (linear a)

instance ToDoc v => Show (Linear v) where show = render . toDoc
instance ToDoc v => Show (Atom v) where show = render . toDoc
