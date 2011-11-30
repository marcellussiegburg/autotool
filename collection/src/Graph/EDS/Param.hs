{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Graph.EDS.Param where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Param = Param
	    { knoten               :: Int
	    , kanten               :: Int
            , solution_size_range :: (Int, Int)
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Param])

p0 :: Param
p0 = Param { knoten               = 10
	   , kanten               = 20
           , solution_size_range = (3, 7)
	   }
