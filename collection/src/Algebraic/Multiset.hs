{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Algebraic.Multiset where

import qualified Algebraic.Set.Multi as M

import qualified Autolib.TES.Binu as B

import           Algebraic2.Class
import Algebraic2.Instance as AI
import Condition

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Reader
import Autolib.Size
import Autolib.Reporter
import Autolib.FiniteMap

import Data.Typeable

data Algebraic_Multiset = Algebraic_Multiset 
    deriving ( Read, Show, Typeable )

derives [makeReader][''Algebraic_Multiset]

data Restriction = None deriving Typeable
derives [makeReader,makeToDoc][''Restriction]
instance Condition Restriction (M.Multiset a) 


instance Algebraic Algebraic_Multiset () ( M.Multiset M.Identifier ) where
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
	let d = M.symmetric_difference a b
            ok = M.null d
	when ( not ok ) $ reject $ vcat 
             [ text "Nein, die symmetrische Differenzmenge ist"
	     , nest 4 $ toDoc d
	     ]
	return ok


    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "A + (B & C)"

    roll_value tag i = 
        M.roll (map (M.Identifier . return) ['p' .. 'u' ]) 5

    default_context tag = ()

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = AI.Make
        { target = read "{p:2,q:3}"
	, context = ()
          , description = Nothing
	  , operators = default_operators tag
	  , predefined = listToFM 
	      [ (read "A", read "{p:1, q:2}" )
	      , (read "B", read "{q:1, r:3}" )
	      , (read "C", read "{p:2, s:4}" )
	      ]		  
          , max_size = 12
	}

    default_operators tag = bops 

