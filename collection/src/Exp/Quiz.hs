{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DatatypeContexts #-}

module Exp.Quiz where

import Autolib.NFA

import qualified NFA.Property as A
import qualified Exp.Property as E

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

import CSP.Property (Iteration(..))

data NFAC c Int => 
     Quiz c = Quiz { generate :: [ A.Property c ]
		 , solve    :: [ E.Property c ]
		 }
            | CSP { process_alphabet :: [c]
                  , process_size :: Int
                  , how_to_iterate :: Iteration
                  ,  generator_repeats :: Int
                 , solve ::  [ E.Property c ]
                 }
            deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Quiz])

example :: Quiz Char
example = Quiz { generate = [ A.Alphabet $ mkSet "ab"
			    , A.Max_Size 5
			    ]
	       , solve    = [ E.Alphabet $ mkSet "ab"
			    , E.Simple
			    ]
	       }

example_csp :: Quiz Char
example_csp = CSP 
   { process_alphabet = "ab"
   , process_size = 5
   , generator_repeats = 100 
   , how_to_iterate = Iteration_Star
   , solve =  [ E.Alphabet $ mkSet "ab"
			    , E.Simple
			    ]
   }           
   
   
