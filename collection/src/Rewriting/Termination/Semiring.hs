{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}

module Rewriting.Termination.Semiring where

import Rewriting.Termination.Domains

class Semiring s where
    zero :: s
    one :: s
    plus :: s -> s -> s
    times :: s -> s -> s
    positive :: s -> Bool
    strictly_greater :: s -> s -> Bool
    weakly_greater :: s -> s -> Bool

instance Semiring Fuzzy where
    zero = FuzzyPinf
    one = FuzzyMinf
    plus = min
    times = max
    positive f = f /= FuzzyPinf
    strictly_greater = (>)
    weakly_greater = (>)

instance Semiring Tropical where
    zero = TropicalInf
    one = TropicalFinite 0
    plus = min
    times a b = case (a,b) of
        (TropicalFinite f, TropicalFinite g) -> TropicalFinite (f+g)
        _ -> TropicalInf
    positive f = case f of TropicalFinite f -> f >= 0 ; _ -> False
    strictly_greater = (>)
    weakly_greater = (>)

instance Semiring Arctic where
    zero = ArcticInf
    one = ArcticFinite 0
    plus = min
    times a b = case (a,b) of
        (ArcticFinite f, ArcticFinite g) -> ArcticFinite (f+g)
        _ -> ArcticInf
    positive f = case f of ArcticFinite f -> f >= 0 ; _ -> False
    strictly_greater = (>)
    weakly_greater = (>)

instance Semiring Natural where
    zero = Natural 0
    one = Natural 1
    plus (Natural a) (Natural b) = Natural (a + b)
    times (Natural a) (Natural b) = Natural (a * b)
    positive (Natural n) = n > 0
    strictly_greater = (>)
    weakly_greater = (>=)


