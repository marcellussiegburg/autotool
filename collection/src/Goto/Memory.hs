module Goto.Memory where

import Autolib.FiniteMap

type Memory = FiniteMap Int Integer

make :: [ ( Int, Integer ) ] -> Memory
make = listToFM

get :: Memory -> Int -> Integer
get m v = lookupWithDefaultFM m 0 v

set :: Memory -> (Int, Integer) -> Memory
set m ( v, k ) = addToFM m v k
