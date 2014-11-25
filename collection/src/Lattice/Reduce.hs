-- | straightforward problem type for introducing lattice base reduction problems:
-- just find a short vector  (not checking which algorithm was used, just the result).

{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}

module Lattice.Reduce where

import qualified Lattice.LLL as L

import Autolib.ToDoc hiding ( char )
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import Data.Hashable
import System.Random

import Challenger.Partial
import Inter.Types
import Inter.Quiz
import Data.Typeable
import Control.Monad ( when )
import Control.Applicative ((<$>))


type Vector a = [ a ]

data Problem = 
     Problem { base :: [ Vector Integer ], bound :: Double }
    deriving Typeable

minpoly scale num x = 
    Problem { base = do
        i <- [ 0 .. num ]
        return $ map round $ scale * x ^ i  : do 
            j <- [ 0 .. num ] ; return $ if i == j then 1 else 0
        , bound = 11
        }

problem0 = minpoly 12345 4 (toRational $ sqrt(2) + sqrt(3))

data Solution = Solution { coefficients :: Vector Integer }
    deriving Typeable

combine :: Vector Integer -> [ Vector Integer ] 
        -> Vector Integer
combine cs vs = 
    foldr1 add $ zipWith scale cs vs

add :: Vector Integer -> Vector Integer -> Vector Integer
add = zipWith (+)
scale :: Integer -> Vector Integer -> Vector Integer
scale c = map (c *)

norm :: Vector Integer -> Double
norm v = sqrt 
       $ fromInteger
       $ sum 
       $ map ( ^ 2 ) v

data Lattice_SVP = Lattice_SVP
    deriving Typeable

derives [makeReader, makeToDoc] [''Problem, ''Solution, ''Lattice_SVP]

instance Show Lattice_SVP where show = render . toDoc

instance OrderScore Lattice_SVP where scoringOrder _ = Decreasing

newtype Oneline a = Oneline a
instance ToDoc a => ToDoc (Oneline a) where
    toDoc (Oneline a) = text $ unwords $ words $ render $ toDoc a

instance Measure Lattice_SVP Problem Solution where
    measure _ p s = 
        let v = combine (coefficients s) (base p)
            n = norm v
            l = round $ logBase 2 n
        in  if all (== 0) v then 0 else l

instance Partial Lattice_SVP Problem Solution where

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
            l = round $ logBase 2 n
            c = if all (== 0) v then 0 else l :: Int
        inform $ vcat
            [ text "linear combination is" </> toDoc (Oneline v)
            , text "with Euclidean norm n = " </> toDoc n
            ]
        when (n > bound p) $ reject 
            $ text "this is larger than the bound" </> toDoc (bound p)
        when (c > 0 ) $ inform $ text "scoring information" </> vcat 
            [ text "log2 n = " </> toDoc l
            ]

make_fixed :: Make
make_fixed = direct Lattice_SVP problem0

-- | for generating instances: we generatae a base that
-- has a short vector and then we do @num_steps@ of random row transformations:
-- add multiple of one row to another.

data Config = Config
        { number_of_vectors :: Int
        , length_of_vectors :: Int
        , short_vector_component_range :: (Integer,Integer)
        , other_vectors_component_range :: (Integer,Integer)
        , num_steps :: Int
        , factor_range :: (Integer,Integer)
        }
    deriving Typeable

config0 = Config
    { number_of_vectors = 5
    , length_of_vectors = 5
    , short_vector_component_range = (-5,5)
    , other_vectors_component_range = (-100,100)
    , num_steps = 25
    , factor_range = (-2,2)
    }

derives [makeReader, makeToDoc] [''Config]

roll :: Config -> IO Problem
roll conf = do
    let dim = length_of_vectors conf
        num = number_of_vectors conf
        vector :: (Integer,Integer) -> IO (Vector Integer)
        vector rng = forM [ 1 .. dim ] $ \ _ -> randomRIO rng
    short <- vector $ short_vector_component_range conf
    others <- forM [ 2 .. num ] $ \ _ -> vector $ other_vectors_component_range conf
    let b0 = short : others
    picks <- forM [ 1 .. num_steps conf ] $ \ i0 -> do
          let i = mod i0 num
          j <- randomRIO (0, num - 2) 
          f <- randomRIO (factor_range conf)
          return (i, j, f)
    let step b (i,j,f) = 
            let (pre, this: post) = splitAt i b
                that = (pre ++ post) !! j
            in  pre ++ add this (scale f that) : post
    return $ Problem
           { base = foldl step b0 picks
           , bound = norm short
           }

-- TODO: roll several times, 
-- take p with largest min of norms
roll_interesting conf = do
        p <- roll conf
        let r = L.fully_reduce $ L.make $ base p
            ns = map L.norm $ L.base r
        if  any (\ n -> toRational n < toRational (bound p)) ns 
            then do -- putStrLn "again"
                    roll_interesting conf
            else return p

instance Generator Lattice_SVP Config Problem where
    generator p conf key = roll_interesting conf
        
instance Project Lattice_SVP Problem Problem where
    project _ p = p

make_quiz :: Make
make_quiz = quiz Lattice_SVP config0
