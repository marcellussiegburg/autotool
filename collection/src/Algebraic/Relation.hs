{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Algebraic.Relation where

import Algebraic.Relation.Restriction
import PL.Struktur ( Predicate (..) )

import Algebraic2.Class
import Algebraic2.Instance as AI
import qualified Algebraic2.Config as AC
import Condition

import qualified Autolib.Relation as R
import qualified Autolib.TES.Binu as B

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Pick (permutations)
import Autolib.Reader
import Autolib.Size
import Autolib.Set
import qualified Data.Set as S
import Autolib.FiniteMap

import qualified Autolib.Reporter.Set

import System.Random
import Data.Typeable
import Data.Ix

data Algebraic_Relation = Algebraic_Relation deriving ( Read, Show, Typeable )

derives [makeReader][''Algebraic_Relation]

instance ( Ord a, ToDoc a ) 
         => Condition Restriction ( Re a ) where
    condition r s = case r of
        Size_Range rng ->
	    assert ( inRange rng $ size s )
		   $ text "Größe im erlaubten Bereich" <+> toDoc rng


---------------------------------------------------------------------------

data Re a = Re { unRe :: R.Type a a } deriving Typeable

instance Ord a => Size ( Re a ) where
    size ( Re r ) = length $ R.pairs r

toPred :: Ord a => Re a -> Predicate a
toPred (Re r) = Predicate $ mkSet $ do
    (x,y) <- R.pairs r
    return [x,y]

fromPred :: Ord a => Predicate a -> Re a
fromPred ( Predicate p ) = Re $ R.make $ do
    [x,y] <- setToList p
    return (x,y)

instance (Ord a,ToDoc a) => ToDoc ( Re a ) where
    toDoc = toDoc . toPred

instance (Ord a,Reader a) => Reader ( Re a) where
    reader = do p <- reader ; return $ fromPred p    

---------------------------------------------------------------------------

instance (ToDoc a, Ord a ) => Ops ( Re a )  where 
    bops = B.Binu
	 { B.nullary = []
	 , B.unary = 
            [ Op { name = "inverse", arity = 1
		 , precedence = Nothing, assoc = AssocNone
		 , inter = lift1R $ R.inverse
		 }
	    ,  Op { name = "transitive_cl", arity = 1
		 , precedence = Nothing, assoc = AssocNone
		 , inter = lift1R $ R.trans
		 }
	    ,  Op { name = "reflexive_cl", arity = 1
		 , precedence = Nothing, assoc = AssocNone
		 , inter = lift1R $ R.reflex
		 }
	    ]
	 , B.binary = 
            [ Op { name = "+", arity = 2
		 , precedence = Just 5, assoc = AssocLeft
		 , inter = lift2R $ R.plus
		 }
	    , Op { name = "-", arity = 2
		 , precedence = Just 5, assoc = AssocLeft
		 , inter = lift2R $ R.difference
		 }
	    ,  Op { name = "&", arity = 2
		 , precedence = Just 6, assoc = AssocLeft
		 , inter = lift2R $ R.intersection
		 }
	    ,  Op { name = ".", arity = 2
		 , precedence = Just 6, assoc = AssocLeft
		 , inter = lift2R $ R.times
		 }
	    ]
	 }

lift1R fun xs = fmap Re $ lift1 fun $ map unRe xs
lift2R fun xs = fmap Re $ lift2 fun $ map unRe xs

---------------------------------------------------------------------------

instance Algebraic Algebraic_Relation 
	   ( Set Integer ) ( Re Integer ) where
    introduce tag con = do
        inform $ vcat
	       [ text "Die folgende Aufgabe bezieht sich auf Relationen"
	       , text "über dem Grundbereich" <+> toDoc con
	       ]

    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluateC tag con bel0 exp = do

	let bel = mapFM ( \ k ( Re v ) ->	
			Re v { R.source = con, R.target = con } ) 
		$ bel0
	
        v <- tfoldB bel inter exp
	inform $ vcat [ text "Der Wert Ihres Terms ist die Relation"
		      , nest 4 $ toDoc v
		      ]
	return v
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
        inform $ text "stimmen die Relationen überein?"
	Autolib.Reporter.Set.eq
	    ( text "Aufgabenstellung", mkSet $ R.pairs $ unRe a )
	    ( text "Einsendung", mkSet $ R.pairs $ unRe b )
	return True


    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "R . S + S"

    default_context tag = mkSet [ 1 .. 5 ]

    roll_value tag c = do
        let perms [] = return []
            perms xs = do
              i <- randomRIO (0, length xs-1)
              let (pre, y : post) = splitAt i xs
              ys <- perms $ pre ++ post
              return $ y : ys
        p <- perms $ do
            x <- S.toList $ AC.context c
            y <- S.toList $ AC.context c
            return (x,y)
        return $ Re $ R.make_on (AC.context c, AC.context c) $ take (S.size $ AC.context c) p

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = AI.Make
        { target = read "{(1,4)}" 
	  , context = default_context tag
          , description = Nothing
	  , operators = default_operators tag
	  , predefined = listToFM 
	      [ (read "R", read "{(1,2),(3,4)}" )
	      , (read "S", read "{(2,3)}" )
	      ]		  
          , max_size = 7
	}

    default_operators tag = bops

