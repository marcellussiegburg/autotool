{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module Lattice.LLL.Compute where

import Data.List ( sort, sortBy, transpose, tails, minimumBy )
import Data.Ratio
import Data.Function ( on )
import Control.Monad ( guard, forM, forM_, when, forever )
import Data.Typeable
import System.Random
import System.IO
import qualified Data.Set as S
import Data.Fixed
import Data.List ( inits )

import Autolib.ToDoc
import Autolib.Reader hiding ( Line )
import Autolib.Size

class M a b where 
    (.*) :: a -> b -> b

instance M Integer Integer where a .* b = a * b

instance M Integer Double where a .* b = fromInteger a * b
instance M Double Double where a .* b = a * b

instance M Integer Rational where a .* b = fromInteger a * b
instance M Rational Rational where a .* b = a * b

dot xs ys = sum $ zipWith (.*) xs ys
norm2 xs = dot xs xs
norm xs = sqrt $ norm2 xs
plus xs ys = zipWith (+) xs ys
neg xs = map negate xs
minus xs ys = plus xs $ neg ys
scale f xs = map (f .*) xs
times a b = for a $ \ u -> for (transpose b) $ \ v -> dot u v

for = flip map

type Vector e = [e]
type Matrix e = [Vector e]

-- | invariants:
-- times (transform s) (back_transform s) == unit s
-- times (transform s) (base0 s) == base s
-- times (back_transform s) (base s) == base0 s

data State =
     State { dim :: Int
           , current :: Matrix Integer
           , orthogonal :: Matrix Rational
           , mu :: Matrix Rational
           , transform :: Matrix Integer
           , reverse_steps :: [ Step ]
           , nsteps :: Int
           , back_transform :: Matrix Integer
           , original :: Matrix Integer
           }

instance Eq (State) where (==) = (==) `on` transform
instance Ord (State) where compare = compare `on` transform

steps = reverse . reverse_steps

range s = [ 0 .. dim s - 1 ]

-- * test cases

q1 = [ [ 1901 , 2914 , 1906 , -3444 ]
    , [ 550 , 845 , 553 , -997 ]
    , [ 1347 , 2368 , 1427 , -2848 ]
    , [ 1061 , 1849 , 1133 , -2202 ]
    ]

q2 = [ [ 1 , 0 , 0 , 0 , 0 , 0 , 0 , 7253 ]
    , [ 0 , 1 , 0 , 0 , 0 , 0 , 0 , 20717 ]
    , [ 0 , 0 , 1 , 0 , 0 , 0 , 0 , 59179 ]
    , [ 0 , 0 , 0 , 1 , 0 , 0 , 0 , 169045 ]
    , [ 0 , 0 , 0 , 0 , 1 , 0 , 0 , 482872 ]
    , [ 0 , 0 , 0 , 0 , 0 , 1 , 0 , 1379306 ]
    , [ 0 , 0 , 0 , 0 , 0 , 0 , 1 , 3939938 ]
    ]

-- * construct initial base

start b = ortho s where 
  s = State
     { dim = length b
     , original = b
     , current = b
     , transform = unit s , back_transform = unit s
     , reverse_steps = [], nsteps = 0
     }

unit s = for (range s) $ \ i ->
         for (range s) $ \ j ->
         if i == j then 1 else 0

ortho s = let o =  gso $ current s
    in s 
    { orthogonal = o
    , mu = for (current s) $ \ bi -> 
           for o $ \ bjo ->
              dot bi bjo / dot bjo bjo
    }

variant s = product 
          $ for (inits $ map norm2 $ orthogonal s) product

gso :: [[Integer]] -> [[Rational]]
gso b = 
    let run b = case b of 
            [] -> []
            x : ys -> 
                x : run ( map (\y -> reduceOrtho_by y x) ys )
    in  run $ map (map fromIntegral) b

reduceOrtho_by this that = 
    let q :: Rational
        q = dot this that / dot that that 
    in  minus this $ scale q that


-- * elementary operations on the base

data Step = Reduce { target :: Int
                   , factor :: Integer
                   , using :: Int }
          | Swap { this :: Int, that :: Int }
    deriving Typeable

derives [makeReader, makeToDoc] [''Step, ''State]

instance Size Step where size _ = 1

apply p s = 
    let (t, t') = case p of
            Reduce {} -> ( reduce_matrix (range s) p
                         , reduce_matrix (range s) p { factor = negate $ factor p }
                         )
            Swap {} ->   ( swap_matrix (range s) p
                         , swap_matrix (range s) p
                         )
        base' = times t (current s) 
    in  ortho $ s
        { current = base'
        , nsteps = succ $ nsteps s
        , reverse_steps = p : reverse_steps s
        , transform = times t (transform s) 
        , back_transform = times (back_transform s) t'
        }

swap_matrix range (Swap {this = i, that = j}) = 
    for range $ \ x -> for range $ \ y -> 
                if x == i then if y == j then 1 else 0
                else if x == j then if y == i then 1 else 0
                else if x == y then 1 else 0

reduce_matrix range (Reduce{target=i,factor=m,using=k}) = 
    for range $ \ x -> for range $ \ y ->
                if x == y then 1 else
                if x == i && y == k then negate m else 0

-- * checking reduction conditions

sizereductions s = do
    i <- range s ; j <- range s ; guard $ j < i
    guard $ not $ sizereductions_check s i j
    return ( Reduce {target=i, factor=round $ mu s !! i !! j
                    , using=j}
           , sizereductions_message s i j
           )

sizereductions_check s i j = 
    let m = mu s !! i !! j
    in  abs m <= 1%2

sizereductions_message s i j = vcat 
    [ text "b_" <> toDoc i <+> equals
           <+> toDoc (V $ current s !! i)
    , text "is not size-reduced w.r.t."
    , text "b_" <> toDoc j <+> equals
           <+> toDoc (V $ current s !! j)
    , text "which has orthogonal part"
    , text "b_" <> toDoc j <> text "^*" <+> equals
           <+> toDoc (V $ orthogonal s !! j)
    ]

swaps s = do
    i <- range s ; let { i' = succ i } ; guard $ i' < dim s
    guard $ not $ shoup_check s i i'
    return ( Swap { this = i, that = i' }
           , shoup_message s i i'
           )

shoup_check s i i' = 
    let bin = norm2 (orthogonal s !! i)
        bi'n = norm2 (orthogonal s !! i')
    in  bin <= 2 * bi'n

shoup_message s i i' = 
    let bin  = norm2 (orthogonal s !! i)
        bi'n = norm2 (orthogonal s !! i')
    in  vcat 
    [ text "Shoup condition does not hold:"
    , text "|b_" <> toDoc i <> text "^*|^2"
                    <+> equals <+> toDoc (centi bin)
    , text "|b_" <> toDoc i' <> text "^*|^2"
                    <+> equals <+> toDoc (centi bi'n)
    ]

-- * for display

instance HasResolution r 
    => ToDoc (Fixed r) where toDoc = text . show

centi :: Rational -> Centi
centi = fromRational

vcent = map centi
mcent = map vcent

newtype Line a = Line { unLine :: a }
line = text . unwords . words . render
instance ToDoc a => ToDoc (Line a) where 
    toDoc = line . toDoc . unLine

newtype Lines a = Lines { unLines :: [a] }
instance ToDoc a => ToDoc (Lines a) where
    toDoc = toDoc . map Line . unLines
    
data V a = V [a]

instance ToDoc (V Integer) where
    toDoc (V xs) = text $ unwords 
       [ show xs, "with norm"
       , show $ centi $ toRational $ sqrt $ fromInteger $ sum $ map (^2) xs
       ]

instance ToDoc (V Rational) where
    toDoc (V xs) = text $ unwords 
       [ show $ map centi xs, "with norm"
       , show $ centi $ toRational $ sqrt $ fromRational $ sum $ map (^2) xs
       ]

instance Nice State where
    nice s = text "State" <+> dutch_set 
        [ text "dim" <+> equals <+> toDoc (dim s)
        , text "current base" <+> equals
          </> toDoc (map V $ current s)
        , text "orthogonally reduced base" <+> equals
          </> toDoc (map V $ orthogonal s)
        , text "variant (for termination)" <+> equals
          </> toDoc (centi $ variant s)
        ]
