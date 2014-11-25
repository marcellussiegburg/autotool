{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}

module Lattice.LLL where

import Data.List ( sort, sortBy, transpose, tails, minimumBy )
import Data.Ratio
import Data.Function ( on )
import Control.Monad ( guard, forM, forM_, when, forever )
import Data.Fixed
import System.Random
import System.IO
import qualified Data.Set as S

class M a b where 
    (*>) :: a -> b -> b

instance HasResolution r => M Integer (Fixed r) where a *> b = fromInteger a * b
instance HasResolution r => M (Fixed r) (Fixed r) where a *> b = a * b
instance M Integer Double where a *> b = fromInteger a * b
instance M Integer Integer where a *> b = a * b
instance M Double Double where a *> b = a * b

dot xs ys = sum $ zipWith (*>) xs ys
ddot xs ys = fromRational $ toRational $ dot xs ys
norm2 xs = ddot xs xs
norm xs = sqrt $ norm2 xs
add xs ys = zipWith (+) xs ys
scale f xs = map (f *>) xs
times a b = for a $ \ u -> for (transpose b) $ \ v -> dot u v

for = flip map

type Vector e = [e]
type Matrix e = [Vector e]
type Base = Matrix Double



-- | invariants:
-- times (transform s) (back_transform s) == unit s
-- times (transform s) (base0 s) == base s
-- times (back_transform s) (base s) == base0 s

data State b =
     State { dim :: Int
           , base :: Matrix b
           , mu :: Matrix Rational
           , transform :: Matrix Integer
           , reverse_steps :: [ Step ]
           , nsteps :: Int
           , back_transform :: Matrix Integer
           , base0 :: Matrix b
           }
    deriving Show

instance Eq (State b) where (==) = (==) `on` transform
instance Ord (State b) where compare = compare `on` transform

steps = reverse . reverse_steps

range s = [ 0 .. dim s - 1 ]

unit s = for (range s) $ \ i ->
                   for (range s) $ \ j ->
                   if i == j then 1 else 0

-- make :: (M b b, M Integer b) => Matrix b -> State b
make b = s where 
  s = State
     { dim = length b
     , base = b
     , base0 = b
     , mu = mu_of b
     , transform = unit s
     , reverse_steps = [], nsteps = 0
     , back_transform = unit s
     }

mu_of b = for b $ \ u -> for b $ \ v -> ddot u v / ddot v v

-- * elementary operations on the base

data Step = Reduce { target :: Int, factor :: Integer, using :: Int }
          | Swap { this :: Int, that :: Int }
    deriving Show

-- | replace  b[i]  with  b[i] - m * b[k]
-- apply :: Step -> State b -> State b
apply p s = 
    let (t, t') = case p of
            Reduce {} -> ( reduce_matrix (range s) p
                         , reduce_matrix (range s) p { factor = negate $ factor p }
                         )
            Swap {} ->   ( swap_matrix (range s) p
                         , swap_matrix (range s) p
                         )
        base' = times t (base s) 
    in  s
        { base = base'
        , mu = mu_of base'
        , transform = times t (transform s) 
        , nsteps = succ $ nsteps s
        , reverse_steps = p : reverse_steps s
        , back_transform = times (back_transform s) t'
        }

swap_matrix range ( Swap { this = i, that = j } ) = 
    for range $ \ x ->
            for range $ \ y -> 
                if x == i then if y == j then 1 else 0
                else if x == j then if y == i then 1 else 0
                else if x == y then 1 else 0

reduce_matrix range (Reduce{target=i,factor=m,using=k}) = 
    for range $ \ x ->
            for range $ \ y ->
                if x == y then 1 else
                if x == i && y == k then negate m else 0

-------------------------------------------------------------------------

-- * several ways for reducing a lattice (for use in a full search)


-- | do a subtraction step if it reduces the norm of the vector.
-- note: subtract or add one vector (not a multiple - need to do this
-- in several steps)
subtract s = do
    i <- range s ; k <- range s
    guard $ not $ all (== 0) $ base s !! k
    guard $ i /= k
    let m = mu s !! i !! k 
    guard $ abs m > 1%2
    let f = if m > 0 then 1 else -1
    return $ apply (Reduce {target=i,factor=f, using=k}) s

-- | do subtraction by the best multiple 
divide s = do
    i <- range s ; k <- range s
    guard $ not $ all (== 0) $ base s !! k
    guard $ i /= k
    let m = mu s !! i !! k 
    guard $ abs m > 1%2
    let f = if m > 0 then truncate (m+1%2)
            else truncate (m-1%2)
    return $ apply (Reduce {target=i,factor= f, using=k}) s

-- | do subtraction by the best multiple 
ordered_divide s = do
    i <- range s ; k <- range s
    guard $ not $ all (== 0) $ base s !! k
    guard $ i < k -- NOTE
    let m = round $ mu s !! i !! k 
    guard $ m /= 0
    return $ apply (Reduce {target=i,factor= m, using=k}) s

-- * full tree search 

reachable :: Ord a => (a -> [a]) -> a -> [a]
reachable f x0 =
    let handle done todo = case S.minView todo of
            Nothing -> []
            Just (t, odo) -> 
                if S.member t done then handle done odo
                else let fresh = S.fromList $ f t
                     in  t : handle (S.insert t done) (S.union fresh odo)
    in  handle S.empty $ S.singleton x0

better s t = (<) `on` shortest
short ss = minimumBy ( compare `on` shortest ) ss
shortest s = minimum $ map norm2 $ base s


inform s = minimum $ map norm2 $ base s

-- | interesting: for this start 
-- reachable subtract finds better vector than reachable divide
b1 =  [[-472,158,-131],[318,264,100],[-190,170,10::Integer]]

b2 = [[79,6,40],[74,-76,-96],[-98,-82,-96]]

b3 = [[26,-54,28],[87,-81,6],[-100,-32,39]]

b4 = [[-20,16,24],[13,-14,-13],[-6,-23,1]]

b5 = [[10,-12,-9],[6,-16,-5],[10,1,-7 :: Integer]]

b6 = [[-2,-6,-3],[-4,-4,4],[10,1,-7 :: Integer]]

b7 = [[-74,63,-33],[97,35,-36],[-35,-28,41 :: Integer]]


run2 dim bnd = do
    let handle top = do
            b <- forM [ 1 .. dim ] $ \ _ -> 
                 forM [ 1 .. dim ] $ \ _ -> 
                 randomRIO (negate bnd , bnd ::Integer)
            let s = fully_reduce $ make b
            when ( nsteps s > top ) $ do
                print s
            handle $ max (nsteps s) top
    handle 0


run1 dim bnd = do
    b <- forM [ 1 .. dim ] $ \ _ -> forM [ 1 .. dim ] $ \ _ -> do
         randomRIO (negate bnd , bnd ::Integer)
    -- print b
    putStr "." ; hFlush stdout
    let s0 = make b
    investigate s0

investigate s0 = do
    -- let f = fully_reduce s0
    -- print ( "fully_reduce",  inform f )
    let s =  reachable Lattice.LLL.subtract s0
        ss = short s
    -- print ( "subtract", length s, inform ss )
    let d = reachable divide s0
        sd = short d
    -- print ( "divide", length d, inform sd )
    when ( shortest ss <  shortest sd ) $ do
        putStrLn $ replicate 50 '-'
        print $ base0 s0
        print ss
        print $ map norm $ base ss
        print sd
        print $ map norm $ base sd
        forM_ (tail s) $ \ s -> investigate $ make $ base s



-- * strategies for applying several of these steps in some order

fully_reduce s = case divide s of
        [] -> s
        s' : _  -> fully_reduce s'

gauss_reduce s = case ordered_divide s of
        [] -> s
        s' : _  -> gauss_reduce s'

lovasz s = do
    (i, u : v : ws) <- zip [0..] $ tails $ base s
    let l = ortho u ws
        r = ortho v ws
    guard $ norm2 l < norm2 r
    return $ apply ( Swap {this=i,that=i+1} ) s

ortho v ws = 
    foldl ( \ v w -> 
        let m = dot v w / dot w w
        in  add v $ scale (negate m) w
      ) v ws

lll s = 
    let g = gauss_reduce s 
    in  case lovasz g of
            [] -> g
            t : _  -> lll t



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

