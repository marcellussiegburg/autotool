{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Algebraic.Set where


import           Algebraic.Nested.Op
import           Algebraic.Nested.Roll (roll)
import           Algebraic.Nested.Restriction
import qualified Algebraic.Nested.Type as Nested
import qualified Autolib.TES.Binu as B

import           Algebraic2.Class
import Algebraic2.Instance as AI
import Condition

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Reader
import Autolib.Size
import Autolib.FiniteMap

import qualified Autolib.Reporter.Set

import Data.Typeable

data Algebraic_Set = Algebraic_Set deriving ( Read, Show, Typeable )

derives [makeReader][''Algebraic_Set]

instance Algebraic Algebraic_Set () ( Nested.Type Integer ) where
    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluate tag bel exp = do
        v <- tfoldB bel inter exp
	inform $ vcat [ text "Der Wert Ihres Terms ist"
		      , nest 4 $ toDoc v
		      ]
	return v
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
        inform $ text "stimmen die Werte überein?"
	let ab = difference a b
	    ba = difference b a
	    err = union ab ba
        let ok = is_empty err 
	when ( not ok ) $ reject $ vcat 
             [ text "Nein, die symmetrische Differenzmenge ist"
	     , nest 4 $ toDoc err
	     ]
	return ok


    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "pow (A + pow (B)) "

    roll_value tag i = roll [1..5] 3

    default_context tag = ()

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = AI.Make
        { target = read "{1,{2}}"
	, context = ()
          , description = Nothing
	  , operators = default_operators tag
	  , predefined = listToFM 
	      [ (read "A", read "{1,3}" )
	      , (read "B", read "{2,3}" )
	      ]		  
          , max_size = 12
	}

    default_operators tag = bops 

