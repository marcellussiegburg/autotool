{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module DPLL.Solve where

import DPLL.Data

import Autolib.Reader
import Autolib.ToDoc
import qualified Autolib.Relation as R

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( partition )
import Control.Applicative ((<$>))

import Data.Typeable

import Debug.Trace

data Step = Decide Literal | Propagate Literal 
          | Backtrack | Backjump_and_learn Clause
          | Success -- Assignment 
          | Fail
    deriving (Eq, Typeable)


type Assignment = [ Literal ]

data Modus = DPLL | DPLL_with_CDCL deriving Typeable

data State =
     State { formula :: CNF
           , assignment :: Assignment 
           , decisions :: [ Literal ]
           , implies :: Maybe [ R.Type Literal Literal ]
           }

derives [makeReader, makeToDoc] [''Step, ''Modus, ''State]

solve :: Modus -> CNF -> [ (State, Step) ]
solve mo cnf = work $ state0 mo cnf

state0 mo cnf = State 
    { formula = cnf, assignment = [], decisions = [] 
    , implies = case mo of
          DPLL -> Nothing
          DPLL_with_CDCL -> Just [ R.empty $ variables cnf ]
    }

work :: State  -> [ (State, Step) ]
work st = 
    let split_clauses = do
            cl <- formula st
            guard $ not $ satisfies (assignment st) cl
            return $ partition (\ l -> not (elem (negate l) (assignment st))) cl
        conflict_clauses = filter ( \ (o,a) -> null o )  split_clauses
        unit_clauses = filter ( \ (o,a) -> 1 == length o ) split_clauses
        v = minimum $ map abs $ concat $ map fst split_clauses
    in  if null split_clauses then success st
        else case conflict_clauses of
             c : _ -> conflict c st
             [] -> case unit_clauses of
                 [] -> decide (negate v) st
                 c : _ -> propagate c st

variables cnf = S.fromList $ map abs $ concat cnf

success st = [ (st, Success)  ]

decide lit st = ( st, Decide lit ) : work  st 
    { decisions = lit : decisions st
    , assignment = lit : assignment st 
    , implies = ( \ i -> head i : i ) <$> implies st
    } 

propagate ( [u], others) st = ( st,  Propagate u ) : work st 
    { assignment = u : assignment st 
    , implies = ( \ i -> ( R.inserts (head i) $ do o <- others; return (negate o,u) ) : tail i)
              <$> implies st
    }

conflict ( [], others ) st = case decisions st of
    [] -> [ (st, Fail ) ]
    d : ds -> case implies st of
        Nothing -> ( st, Backtrack )
         : work st { assignment = (negate d :) $ tail $ dropWhile ( /= d) $ assignment st
                   , decisions = ds 
                   , implies = Nothing 
                   }
        Just i -> 
           let g = R.trans $ head i
               m = R.minima (head i)
               o = R.pre_simages g (S.fromList $ map negate others)
               reason = S.toList $ S.intersection m o
               learnt = map negate reason
               earlier = filter (/= d) reason
           in ( st, Backjump_and_learn learnt ) :
               case dropWhile ( \ d' -> not $ elem d' earlier ) ds of
                 [] ->  work $ state0 DPLL_with_CDCL $ learnt : formula st
                 d' : ds' -> work st 
                       { formula = learnt : formula st
                       , assignment = dropWhile ( /= d' ) $ assignment st
                       , decisions = ds'
                       , implies = Just $ ekat (length ds' + 1) i
                       }

ekat k = reverse . take k . reverse

reduce :: Assignment -> CNF -> CNF
reduce ass cnf = 
      map ( filter ( \ l -> not ( elem (negate l) ass) ) )
    $ filter ( \ cl -> not (satisfies ass cl)) cnf

satisfies :: Assignment -> Clause -> Bool
satisfies ass cl = 
    not $ S.null $ S.intersection ( S.fromList ass) (S.fromList cl)
