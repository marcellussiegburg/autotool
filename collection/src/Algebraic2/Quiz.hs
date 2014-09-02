{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Algebraic2.Quiz where

import qualified Challenger.Partial as P
import Algebraic2.Central
import Algebraic2.Class
import qualified Algebraic2.Config as C
import qualified Algebraic2.Instance as I

import qualified Algebraic.Nested.Type as Nested
import qualified Algebraic.Nested.Op

import Autolib.Reader
import Inter.Types
import Inter.Quiz

import Condition
import Debug

import qualified Autolib.TES.Binu as B
import Autolib.TES.Type

import qualified Autolib.TES.Enum as E
import Autolib.TES.Position
import Autolib.TES.Identifier (mkunary, mkbinary, mknullary)
import Autolib.Choose
import Autolib.Pick
import Autolib.Size
import Autolib.FiniteMap
import qualified Data.Map as M
import qualified Data.Set as S
import Autolib.Util.Zufall
import Data.Maybe (isJust, isNothing )
import Data.List ( maximumBy )
import Data.Function (on)
import Data.IORef

-- for testing:
check0 :: IO (Term Identifier (Op (Nested.Type Integer)))
check0 = roll bops 5 bel0

bel0 = M.fromList
       [ (read "A" :: Identifier, read "{1,{2}}" :: Nested.Type Integer )
       , (read "B", read "{2,{3}}" )  
       ]

------------------------------------------------------------------------

--  NOTE: lazy infinite list
all_small_terms binu0 bel = do
   s <- [ 0 .. ]
   roll_simplified binu0 s bel


------------------------------------------------------------------------

with_nulls binu0 bel = 
    let new_nulls = do
           ( b, v ) <- fmToList bel
	   return $ Op { name = show b, arity = 0, precedence = Nothing
		       , assoc = AssocNone
		       , inter = \ any -> return v
		       }
    in  binu0 { B.nullary = new_nulls ++ B.nullary binu0 }

roll binu s bel = roll_simplified binu s bel

roll_simplified binu0 s bel =
    choose (with_nulls binu0 bel) s

roll_original binu0 s bel = do
    let binu = with_nulls binu0 bel
        no_unaries = binu { B.unary = [] }
    num_uns <- randomRIO ( 0, min 2 s )
    t <- choose no_unaries ( s - num_uns )
    case B.unary binu of
        [] -> return t
	us -> do
	    uns <- sequence $ replicate num_uns $ eins us
	    poke_unaries t uns

poke_unaries t [] = return t
poke_unaries t (u : us) = do
    ( p, s ) <- eins $ positions t
    let t' = poke t ( p, Node u [s] )
    poke_unaries t' us

------------------------------------------------------------------------

internal_roller tag context ops beleg size rests rounds = do
        counter <- newIORef rounds
        ( t, Just x, c )  <- do
	      t <- roll  ops size beleg
	      c <- readIORef counter
	      writeIORef counter $ pred c
	      debug $ unlines 
		    [ show c, show t ]			   
	      return ( t, result $ evaluateC tag context beleg t, c )
	  `repeat_until` \ ( t, mx, c ) -> case mx of
	      _  | c < 0 -> error $ unlines
		     [ "generator could not produce instance,"
		     , "Tutor: perhaps remove some restrictions."
		     , "Student: report error to Tutor."
		     ] 
	      Nothing -> False
	      Just x  -> isJust 
                  $ result $ investigate rests x
	return ( t, x )

instance ( Condition c a,  Algebraic tag context a, Reader tag )
    => Generator (T tag) ( C.Type context c a ) ( I.Type context a , Exp a ) where
    generator (T tag) conf key = do
        its <- forM [ 1 .. C.instance_candidates conf  ] $ \ i -> do
            (i,t) <- run_generator (T tag) conf key
            let small_nonsolutions = do
                  s <- take (C.small_solution_candidates conf )
                    $ all_small_terms (I.operators i) (I.predefined i)
                  guard $ isNothing $ result $ P.total (T tag) i s
            return (length small_nonsolutions, (i,t))      
        return $ snd $ maximumBy (compare `on` fst) its


run_generator (T tag) conf key = do
        pre <- fmap listToFM $ sequence $ do
	    ( b, mv ) <- fmToList $ C.predefined conf
	    return $ case mv of
		    Right v -> return ( b, v )
		    Left i -> case i of
                      C.Value -> do
                         x <- roll_value tag conf
                         return (b, x)
                      C.Formula -> do
			 ( t, x ) <- internal_roller tag ( C.context conf )
			     ( C.operators_in_instance conf ) emptyFM
			     ( C.max_formula_size_for_predefined conf )
			     ( C.restrictions_for_predefined conf )
			     ( C.solution_candidates conf )
                         return (b, x) 

        ( t, x ) <- internal_roller tag ( C.context conf )
	       ( C.operators_in_instance conf ) pre
	       ( C.max_formula_size_for_instance conf )
	       ( C.restrictions_for_instance conf ) 
	       ( C.solution_candidates conf )

	return ( I.Make
		         { I.target = x
			 , I.context = C.context conf
		         , I.description = case C.information conf of
			     C.Value -> Nothing
			     C.Formula -> Just $ show t
		         , I.operators = C.operators_in_solution conf
			 , I.predefined = pre
		         , I.max_size = max ( size t )
				  ( C.max_formula_size_for_solution conf )
		         }
		       , t
		       )


instance Algebraic tag context a 
      =>  Project (T tag) ( I.Type context a, Exp a ) ( I.Type context a ) where
    project (T tag) (i, f) = i

make :: ( Condition c a, Algebraic tag context a, Reader tag )
     => tag -> Make
make tag = quiz (T tag) $ C.Make
     { C.context = default_context tag
     , C.max_formula_size_for_instance = 6
	 , C.operators_in_instance = default_operators tag
	 , C.operators_in_solution = default_operators tag
	 , C.restrictions_for_instance = suggest
	 , C.restrictions_for_predefined = suggest
	 , C.predefined = listToFM [ ( read "A", Left C.Value ) ]
	 , C.max_formula_size_for_predefined = 3
         , C.information = C.Value
	 , C.max_formula_size_for_solution = 10
         , C.solution_candidates = 10000
         , C.instance_candidates = 100
         , C.small_solution_candidates = 1000
	 }

