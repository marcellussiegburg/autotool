{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}

module Rewriting.Termination.Semiring where

import Rewriting.Termination.Domains

class (Eq s) => Semiring s where
    strict_addition :: s -> Bool -- ^ with dummy argument
    zero :: s
    one :: s
    plus :: s -> s -> s
    times :: s -> s -> s
    is_zero :: s -> Bool
    positive :: s -> Bool
    strictly_greater :: s -> s -> Bool
    weakly_greater :: s -> s -> Bool

instance Semiring Fuzzy where
    strict_addition _ = False
    zero = FuzzyPinf
    one = FuzzyMinf
    plus = min
    times = max
    is_zero f = f == FuzzyPinf
    positive f = f /= FuzzyPinf
    strictly_greater = (>)
    weakly_greater = (>)

instance Semiring Tropical where
    strict_addition _ = False
    zero = TropicalInf
    one = TropicalFinite 0
    plus = min
    times a b = case (a,b) of
        (TropicalFinite f, TropicalFinite g) -> TropicalFinite (f+g)
        _ -> TropicalInf
    is_zero f = f == TropicalInf
    positive f = case f of TropicalFinite f -> f >= 0 ; _ -> False
    strictly_greater = (>)
    weakly_greater = (>=)

instance Semiring Arctic where
    strict_addition _ = False
    zero = ArcticInf
    one = ArcticFinite 0
    plus = min
    times a b = case (a,b) of
        (ArcticFinite f, ArcticFinite g) -> ArcticFinite (f+g)
        _ -> ArcticInf
    is_zero f = f == ArcticInf
    positive f = case f of ArcticFinite f -> f >= 0 ; _ -> False
    strictly_greater = (>)
    weakly_greater = (>=)

instance Semiring Natural where
    strict_addition _ = True
    zero = Natural 0
    one = Natural 1
    plus (Natural a) (Natural b) = Natural (a + b)
    times (Natural a) (Natural b) = Natural (a * b)
    is_zero (Natural n) = n == 0
    positive (Natural n) = n > 0
    strictly_greater = (>)
    weakly_greater = (>=)


