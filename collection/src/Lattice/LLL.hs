module Lattice.LLL where

import Data.List ( sortBy )
import Data.Function ( on )
import Control.Monad ( guard )

dot xs ys = sum $ zipWith (*) xs ys
norm xs = sqrt $ dot xs xs
add xs ys = zipWith (+) xs ys
scale f xs = map (f *) xs

type Vector = [Double]

-- | reduce v by adding/subtracting an integer multiple of u
reduce :: Vector -> Vector -> Vector
reduce u v = 
    let p = round ( dot u v / dot u u )
    in  add (scale (fromIntegral $ negate p) u) v

-- | reduce v by adding/subtracting an arbitrary multiple of u
project :: Vector -> Vector -> Vector
project u v = 
    let p = dot u v / dot u u 
    in  add (scale ( negate p) u) v


sizereduce :: Base -> Base
sizereduce [] = []
sizereduce (u : vs) = 
    let ws = sizereduce vs 
    in  foldr reduce u ws : ws

gramschmidt :: Base -> Base
gramschmidt [] = []
gramschmidt (u : vs) = 
    let ws = gramschmidt vs 
    in  foldr project u ws : ws

swap i b = 
    let (pre, u:v:post) = splitAt i b
    in pre ++ v : u : post

lovasz :: Base -> Base
lovasz b = 
    let b' = gramschmidt b
        bad = do i <- [0 .. length b - 2]
                 guard $ not $ norm (b' !! i) < norm (b' !! (i+1))
                 return i
    in  case bad of
            [] -> b
            i : _ -> lovasz $ sizereduce $ swap i b

type Base = [[Double]]

b0 :: Base
b0 = 
    [ [ 41510.633096 , 39145.275121 , 27701.435729 , 2533.148154 , 11181.680102 ]
    , [ 4116.295922 , 3878.627857 , 2764.660511 , 290.28102 , 1153.834423 ]
    , [ 2393.482234 , 2457.216087 , 457.625776 , 2641.923659 , 2418.560096 ]
    , [ 8639.279815 , 8040.600185 , 5637.630487 , 451.01875 , 2128.060412 ]
    , [ 41873.275037 , 38973.133269 , 27325.72408 , 2185.419842 , 10315.19886 ]
    ]
