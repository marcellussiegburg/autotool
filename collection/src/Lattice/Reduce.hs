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
import Data.Typeable
import Control.Monad ( when )

instance HasResolution p => ToDoc (Fixed p) where
    toDoc = text . showFixed False

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

minpoly scale num x = 
    Problem { base = do
        i <- [ 0 .. num ]
        return $ (fromRational scale * (fromRational x ^ i )) : do 
            j <- [ 0 .. num ] ; return $ if i == j then 1 else 0
        , bound = 10
        }

problem0 = minpoly 175 4 (toRational $ sqrt(2) + sqrt(3))

data Solution = Solution { coefficients :: Vector Integer }

combine :: HasResolution p
        => Vector Integer -> [ Vector (Fixed p) ] 
        -> Vector (Fixed p)
combine cs vs = 
    foldr1 (zipWith (+)) $ zipWith ( \ c -> map (fromInteger c *) ) cs vs

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

instance OrderScore Lattice_Reduce where scoringOrder _ = Increasing

newtype Oneline a = Oneline a
instance ToDoc a => ToDoc (Oneline a) where
    toDoc (Oneline a) = text $ unwords $ words $ render $ toDoc a

instance Measure Lattice_Reduce Problem Solution where
    measure _ p s = 
        let v = combine (coefficients s) (base p)
            n = truncate $ logBase 10 ( norm v ) 
                         + logBase 10 ( fromInteger $ resolution ( undefined :: Fixed E6 ))
        in  if all (== 0) v then 0 else n

instance Partial Lattice_Reduce Problem Solution where

    describe _ p = vcat
        [ text "Find an integer linear combination of these vectors"
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
            
    total _ p s = do
        let v = combine (coefficients s) (base p)
            n = truncate $ logBase 10 ( norm v ) 
                         + logBase 10 ( fromInteger $ resolution ( undefined :: Fixed E6 ))
        in  if all (== 0) v then 0 else n
        
