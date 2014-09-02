{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# language DatatypeContexts #-}
{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module Rewriting.Termination.Multilinear where

import Rewriting.Termination.Semiring
import Rewriting.Termination.Matrix

import Autolib.Reader
import Autolib.ToDoc
import Data.List ( transpose )
import Data.Typeable

data Semiring d => Multilinear d = 
     Multilinear { absolute :: Matrix d
                 , coefficients :: [Matrix d] 
                 }
    deriving ( Eq, Typeable )

derives [makeReader, makeToDoc] [''Multilinear]

substitute :: Semiring d 
           => Multilinear d -> [Multilinear d] -> Multilinear d
substitute f gs = 
    Multilinear { absolute = foldr mplus (absolute f) 
       $ zipWith mtimes (coefficients f) (map absolute gs)
                , coefficients = map ( foldr1 mplus )
                           $ transpose 
                           $ map (zipWith mtimes (coefficients f)) 
                           $ map coefficients gs
                }

projection :: Semiring d 
           => Int -> Int -> Int -> Multilinear d
projection from to dim =
    Multilinear { absolute = zero_matrix (dim, 1)
                , coefficients = for [1..from] $ \ k -> 
                      if k == to then unit_matrix dim
                      else zero_matrix (dim,dim)
                }
                      
