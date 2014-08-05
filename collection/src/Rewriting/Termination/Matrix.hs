{-# language DeriveDataTypeable #-}

module Rewriting.Termination.Matrix where

import Rewriting.Termination.Semiring

import Autolib.Reader
import Autolib.ToDoc

import Control.Monad ( when )
import Control.Applicative ( (<$>) )
import Data.Typeable

type Vector d = [d]

data Matrix d = Matrix { dim :: (Int,Int) , contents :: [[d]] }
    deriving ( Eq, Typeable )

zero_matrix (h,w) = 
    Matrix { contents = replicate h $ replicate w $ zero
           , dim = (h,w)
           }

unit_matrix d = 
    Matrix { contents = for [0..d] $ \ x -> 
                        for [0..d] $ \ y -> 
                        if x==y then one else zero
           , dim = (d,d)
           }

for = flip map

instance ToDoc d => ToDoc (Matrix d) where
    toDoc = toDoc . contents
instance (Semiring d, Reader d) => Reader (Matrix d) where
    reader = 
            do my_reserved "Zero" ; zero_matrix <$> reader
        <|> do my_reserved "Unit" ; unit_matrix <$> reader
        <|> do xss <- reader
               if null xss then fail "matrix height cannot be zero"
               else do let ls = map length xss
                           (lo,hi) = (minimum ls, maximum ls)
                       if (lo /= hi) 
                       then fail "matrix rows differ in length"
                       else return $ Matrix { contents = xss 
                                            , dim = (length ls, lo)
                                            }
