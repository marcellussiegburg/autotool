{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Convert.Type where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

import qualified Convert.Input

import qualified CSP.Trace

import Autolib.Exp.Inter
import Autolib.Exp
import Autolib.NFA hiding ( alphabet )
import Autolib.Set

data Convert =
     Convert { name :: Maybe [String] 
                       -- ^ if Nothing, use (show input)
             , input :: Convert.Input.Input Char
	     }
    deriving ( Typeable , Show )

$(derives [makeReader, makeToDoc] [''Convert])

form :: Convert -> Doc
form conv = case name conv of
    Nothing  -> Convert.Input.lang $ input conv
    Just css -> vcat $ map text css

eval :: Set Char -> Convert -> NFA Char Int
eval alpha conv = case input conv of
    Convert.Input.NFA aut -> aut
    Convert.Input.Exp exp -> 
        inter ( std_sigma $ setToList alpha ) exp
    Convert.Input.Process p -> 
        Autolib.NFA.minimize0 
         $ Autolib.NFA.normalize  
         $ CSP.Trace.auto p


