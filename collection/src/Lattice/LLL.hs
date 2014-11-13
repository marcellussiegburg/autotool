{-# language MultiParamTypeClasses #-}

module Lattice.LLL where

import Data.List ( sortBy, transpose, tails )
import Data.Function ( on )
import Control.Monad ( guard )

class M a b where 
    (*>) :: a -> b -> b

instance M Integer Double where a *> b = fromInteger a * b
instance M Integer Integer where a *> b = a * b
instance M Double Double where a *> b = a * b

dot xs ys = sum $ zipWith (*>) xs ys
norm2 xs = dot xs xs
norm xs = sqrt $ norm2 xs
add xs ys = zipWith (+) xs ys
scale f xs = map (f *>) xs
times a b = for a $ \ u -> for (transpose b) $ \ v -> dot u v

for = flip map

type Vector e = [e]
type Matrix e = [Vector e]
type Base = Matrix Double


b0 :: Base
b0 = 
    [ [ 41510.633096 , 39145.275121 , 27701.435729 , 2533.148154 , 11181.680102 ]
    , [ 4116.295922 , 3878.627857 , 2764.660511 , 290.28102 , 1153.834423 ]
    , [ 2393.482234 , 2457.216087 , 457.625776 , 2641.923659 , 2418.560096 ]
    , [ 8639.279815 , 8040.600185 , 5637.630487 , 451.01875 , 2128.060412 ]
    , [ 41873.275037 , 38973.133269 , 27325.72408 , 2185.419842 , 10315.19886 ]
    ]

-- | invariants:
-- times (transform s) (back_transform s) == unit s
-- times (transform s) (base0 s) == base s
-- times (back_transform s) (base s) == base0 s

data State =
     State { dim :: Int
           , base :: Matrix Double
           , mu :: Matrix Double
           , transform :: Matrix Integer
           , back_transform :: Matrix Integer
           , base0 :: Matrix Double 
           }
    deriving Show

range s = [ 0 .. dim s - 1 ]

unit s = for (range s) $ \ i ->
                   for (range s) $ \ j ->
                   if i == j then 1 else 0

make :: Matrix Double -> State
make b = s where 
  s = State
     { dim = length b
     , base = b
     , base0 = b
     , mu = mu_of b
     , transform = unit s
     , back_transform = unit s
     }

mu_of b = for b $ \ u -> for b $ \ v -> dot u v / dot v v

-- | replace  b[i]  with  b[i] - m * b[k]
modify :: (Int,Integer,Int) -> State -> State
modify (i, m, k) s | i /= k = 
    let t = trans (range s) (i,m,k)
        t' = trans (range s) (i, negate m, k)
        base' = times t (base s) 
    in  s
        { base = base'
        , mu = mu_of base'
        , transform = times t (transform s) 
        , back_transform = times (back_transform s) t'
        }

swap i s = 
    let t = swap_matrix (range s) (i, i+1)
        t' = t
        base' = times t (base s) 
    in  s
        { base = base'
        , mu = mu_of base'
        , transform = times t (transform s) 
        , back_transform = times (back_transform s) t'
        }

swap_matrix range (i,j) = 
    for range $ \ x ->
            for range $ \ y -> 
                if x == i then if y == j then 1 else 0
                else if x == j then if y == i then 1 else 0
                else if x == y then 1 else 0

trans range (i,m,k) = 
    for range $ \ x ->
            for range $ \ y ->
                if x == y then 1 else
                if x == i && y == k then negate m else 0

bad_gauss s = do 
            i <- range s ; k <- range s
            guard $ i /= k
            let m = round $ mu s !! i !! k      
            guard $ m /= 0
            return (i,m,k)

gauss_reduce_step s = case bad_gauss s of
            [] -> Nothing
            b : _ -> Just $ modify b s

gauss_reduce s = case
    gauss_reduce_step s of
        Nothing -> s
        Just s' -> gauss_reduce s'

bad_lovasz s = do
    (i, u : v : ws) <- zip [0..] $ tails $ base s
    let l = ortho u ws
        r = ortho v ws
    guard $ norm2 l < norm2 r
    return i

ortho v ws = 
    foldl ( \ v w -> 
        let m = dot v w / dot w w
        in  add v $ scale (negate m) w
      ) v ws

lovasz_reduce_step s = case bad_lovasz s of
    [] -> Nothing
    i : _ -> Just $ swap i s

lll s = 
    let g = gauss_reduce s 
    in  case lovasz_reduce_step g of
            Nothing -> g
            Just s' -> lll s'



{-

-- | reduce v by adding/subtracting an integer multiple of u
reduce :: (RealFrac e, Fractional e, Num e)
       => Vector e -> Vector e -> Vector e
reduce u v = 
    let p = round ( dot u v / dot u u )
    in  add (scale (fromIntegral $ negate p) u) v

-- | reduce v by adding/subtracting an arbitrary multiple of u
project :: (Fractional e, Num e)
        => Vector e -> Vector e -> Vector e
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

-}

