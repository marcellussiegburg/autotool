-- | straightforward problem type for introducing lattice base reduction problems:
-- just find a short vector  (not checking which algorithm was used, just the result).
-- need to be a bit careful here: cannot use Double because a solution could cheat:
-- multiply everything by really large amounts and get extinction of
-- precision upon subtraction (resulting in a "zero" vector).
-- Hence, use fixed precision numerals (they internally use Integer).

{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}

module Lattice.Reduce where

import Autolib.ToDoc hiding ( char )
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import Data.Hashable
import System.Random

import Data.Fixed
import Data.Char (isDigit)

import Challenger.Partial
import Inter.Types
import Inter.Quiz
import Data.Typeable
import Control.Monad ( when )
import Control.Applicative ((<$>))

instance HasResolution p => ToDoc (Fixed p) where
    toDoc = text . showFixed True

instance HasResolution p => Reader (Fixed p) where
    reader = do
        sign <- option '+' ( char '+' <|> char '-' )
        pre <- many1 digit
        post <- option "" $ do char '.' ; many1 digit
        my_whiteSpace 
        let res = resolution ( undefined :: Fixed p ) 
            log10 n = if n == 1 then 0 else succ (log10 (div n 10))
            places = log10 res
        when ( fromIntegral (length post) > places ) 
           $ fail "number cannot be represented exactly"
        let n = read $ pre ++ post ++ replicate (places - length post) '0' 
        return $ MkFixed n

type Vector a = [ a ]

data Problem = 
     Problem { base :: [ Vector (Fixed E6) ], bound :: Double }
    deriving Typeable

minpoly scale num x = 
    Problem { base = do
        i <- [ 0 .. num ]
        return $ (fromRational scale * (fromRational x ^ i )) : do 
            j <- [ 0 .. num ] ; return $ if i == j then 1 else 0
        , bound = 11
        }

problem0 = minpoly 12345 4 (toRational $ sqrt(2) + sqrt(3))

data Solution = Solution { coefficients :: Vector Integer }
    deriving Typeable

combine :: HasResolution p
        => Vector Integer -> [ Vector (Fixed p) ] 
        -> Vector (Fixed p)
combine cs vs = 
    foldr1 add $ zipWith scale cs vs

add :: HasResolution p
    => Vector (Fixed p) -> Vector (Fixed p) -> Vector (Fixed p) 
add = zipWith (+)
scale c = map (fromInteger c *)

norm :: HasResolution p 
     => Vector (Fixed p) -> Double
norm v = sqrt 
       $ fromRational 
       $ sum 
       $ map ( \ x -> toRational x ^ (2 :: Int)) v

data Lattice_Reduce = Lattice_Reduce
    deriving Typeable

derives [makeReader, makeToDoc] [''Problem, ''Solution, ''Lattice_Reduce]

instance Show Lattice_Reduce where show = render . toDoc

instance OrderScore Lattice_Reduce where scoringOrder _ = Decreasing

newtype Oneline a = Oneline a
instance ToDoc a => ToDoc (Oneline a) where
    toDoc (Oneline a) = text $ unwords $ words $ render $ toDoc a

instance Measure Lattice_Reduce Problem Solution where
    measure _ p s = 
        let v = combine (coefficients s) (base p)
            n = norm v
            d = n * fromInteger ( resolution ( undefined :: Fixed E6 ))
            l = round $ logBase 2 d
        in  if all (== 0) v then 0 else l

instance Partial Lattice_Reduce Problem Solution where

    describe _ p = vcat
        [ text "Find an nonzero integer linear combination of these vectors"
        , nest 4 $ toDoc $ map Oneline $ base p
        , text "with Euclidean norm <=" <+> toDoc (bound p)
        ]

    initial _ p = 
        let g = mkStdGen $ hash $ render $ toDoc p
            cs = map ( \ i -> mod i 100 - 50 ) 
               $ take (length $ base p) 
               $ randoms g
        in  Solution { coefficients = cs }

    partial _ p s = do
        when (length (coefficients s) /= length (base p)) $ reject $ vcat
            [ text "number of coefficients" 
                  <+> parens (toDoc (length $ coefficients s))
            , text "is not equal to number of base vectors" 
                  <+> parens (toDoc (length $ base p))
            ]
        when (all (== 0) $ coefficients s) $ reject
            $ text "at least one coefficient must be non-zero"
            
    total _ p s = do
        let v = combine (coefficients s) (base p)
            n = norm v
            d = n * fromInteger ( resolution ( undefined :: Fixed E6 ))
            l = round $ logBase 2 d
            c = if all (== 0) v then 0 else l :: Int
        inform $ vcat
            [ text "linear combination is" </> toDoc (Oneline v)
            , text "with Euclidean norm" </> toDoc n
            ]
        when (n > bound p) $ reject 
            $ text "this is larger than the bound" </> toDoc (bound p)
        when (c > 0 ) $ inform $ text "scoring information" </> vcat 
            [ text "d = norm / resolution = " </> toDoc d
            , text "log2 d = " </> toDoc l
            ]

make_fixed :: Make
make_fixed = direct Lattice_Reduce problem0

-- | for generating instances: we generatae a base that
-- has a short vector and then we do @num_steps@ of random row transformations:
-- add multiple of one row to another.

data Config = Config
        { number_of_vectors :: Int
        , length_of_vectors :: Int
        , short_vector_norm_at_most :: Double
        , other_vectors_norm_at_least :: Double
        , num_steps :: Int
        , factor_size :: Integer
        }
    deriving Typeable

config0 = Config
    { number_of_vectors = 5
    , length_of_vectors = 5
    , short_vector_norm_at_most = 0.2
    , other_vectors_norm_at_least = 100
    , num_steps = 10
    , factor_size = 10
    }

derives [makeReader, makeToDoc] [''Config]

roll :: Config -> IO Problem
roll conf = do
    let dim = length_of_vectors conf
        num = number_of_vectors conf
        sh = short_vector_norm_at_most conf 
           / sqrt (fromIntegral dim)
        vector :: Double -> IO (Vector (Fixed E6))
        vector b = forM [ 1 .. dim ] $ \ _ -> 
            fromRational <$> toRational <$> ( randomRIO (negate b, b) :: IO Double )
    short <- vector sh
    others <- forM [ 2 .. num ] $ \ _ -> vector $ other_vectors_norm_at_least conf
    let b0 = short : others
    picks <- forM [ 1 .. num_steps conf ] $ \ i0 -> do
          let i = mod i0 num
          j <- randomRIO (0, num - 2) 
          f <- randomRIO (negate $ factor_size conf, factor_size conf)
          return (i, j, f)
    let step b (i,j,f) = 
            let (pre, this: post) = splitAt i b
                that = (pre ++ post) !! j
            in  pre ++ add this (scale f that) : post
    return $ Problem
           { base = foldl step b0 picks
           , bound = 2 * norm short
           }

instance Generator Lattice_Reduce Config Problem where
    generator p conf key = roll conf
        
instance Project Lattice_Reduce Problem Problem where
    project _ p = p

make_quiz :: Make
make_quiz = quiz Lattice_Reduce config0
