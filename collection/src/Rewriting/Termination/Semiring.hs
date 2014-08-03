{-# language GeneralizedNewtypeDeriving #-}

module Rewriting.Termination.Semiring where

import Autolib.Reader
import Autolib.ToDoc

import Control.Monad ( when )

class Semiring s where
    positive :: s -> Bool
    zero :: s
    one :: s
    plus :: s -> s -> s
    times :: s -> s -> s
    strictly_greater :: s -> s -> Bool
    weakly_greater :: s -> s -> Bool

newtype Natural = Natural Integer
    deriving ( Eq, Ord, Enum, Num )
    
instance ToDoc Natural where
    toDoc (Natural i) = toDoc i

instance Reader Natural where
    reader = do 
        i <- reader
        when (i < 0) $ fail "number cannot be negative"
        return $ Natural i

type Vector d = [d]
type Matrix d = [[d]]

