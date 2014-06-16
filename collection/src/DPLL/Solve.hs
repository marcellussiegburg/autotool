{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module DPLL.Solve where

import DPLL.Data

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size
import qualified Autolib.Relation as R

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( partition )
import Control.Applicative ((<$>))

import Data.Typeable

import Debug.Trace


data Step = Decide Literal
          | Propagate { use :: Clause, obtain :: Literal }
          | SAT
          | Conflict Clause
          | Backtrack 
          | Backjump { to_level :: Int, learn :: Clause }
          | UNSAT
    deriving (Eq, Typeable)

instance Size Step where size = const 1

data Reason = Decision 
            | Propagation { antecedent :: Clause }
    deriving (Eq, Typeable)

data Info = 
     Info { value :: Bool
          , level :: Int
          , reason :: Reason
          }
    deriving (Eq, Typeable)

data State =
     State { formula :: CNF
           , decision_level :: Int
           , assignment :: M.Map Variable Info
           , conflict_clause :: Maybe Clause
           }

state0 cnf = State 
    { formula = cnf
    , decision_level = 0
    , assignment = M.empty
    , conflict_clause = Nothing
    }

data Modus = Modus
     { require_complete_propagation :: Bool
     , clause_learning :: Bool
     , variable_elimination :: Bool
     }
    deriving (Eq, Typeable)

modus0 :: Modus
modus0 = Modus
     { require_complete_propagation = False
     , clause_learning = False
     , variable_elimination = False
     }

derives [makeReader, makeToDoc] [''Step, ''Reason, ''Info, ''State, ''Modus]


execute :: Modus -> CNF -> [Step] -> Reporter State
execute mo cnf steps = foldM ( work mo ) (state0 cnf)  steps


must_be_unassigned a l = 
      maybe ( return () ) 
            ( const $ reject $ text "literal" <+> toDoc l <+> text "is already assigned")
  $ M.lookup (variable l) $ assignment a

assert_no_conflict a = 
      maybe ( return () )
            ( const $ reject $ text "must handle conflict first" )
   $  conflict_clause a

assert_conflict :: State -> ( Clause -> Reporter r ) -> Reporter r
assert_conflict a cont = case conflict_clause a of
    Nothing -> reject $ text "must find conflict first"
    Just cl -> cont cl

reset_dl dl a = a 
    { assignment = M.filter ( \ i -> level i <= dl ) $ assignment a
    , decision_level = dl
    }

evaluate_literal a l = do
    let f = if positive l then id else not
    f <$> value <$> M.lookup (variable l) a

work :: Modus -> State  -> Step -> Reporter State
work mo a s = do
    inform $ vcat [ toDoc a, toDoc s ]
    case s of
        Decide l -> do
            assert_no_conflict a
            must_be_unassigned a l
            when (positive l) $ reject $ text "initial decision must be negative"
            let dl = succ $ decision_level a
            return $ a 
                { decision_level = dl
                , assignment = M.insert (variable l) 
                    (Info { value = (positive l)
                          , level = dl
                          , reason = Decision } ) $ assignment a
                }
        Propagate { use = cl, obtain = l } -> do
            assert_no_conflict a
            must_be_unassigned a l
            when (not $ elem cl $ formula a) $ reject $ text "the cnf does not contain this clause"
            when (not $ elem l cl) $ reject $ text "the clause does not contain this literal"
            let ( me, others ) = partition (== l) cl
            forM_ others $ \ o -> case (evaluate_literal (assignment a) o) of
                Nothing -> reject $ text "the clause is not unit, because" 
                        <+> toDoc o <+> text "is unassigned"
                Just True -> reject $ text "the clause is not unit, because" 
                        <+> toDoc o <+> text "is true"
                Just False -> return ()
            let dl = decision_level a
            return $ a 
               { assignment = M.insert (variable l) (Info {
                   value = positive l, level = dl, reason = Propagation cl}) $ assignment a
               }
        SAT -> do
            assert_no_conflict a
            forM_ (formula a) $ \ cl -> do
                let vs = map (\ l -> (l, evaluate_literal (assignment a) l)) cl
                    sat = any ( \ (l,v) -> v == Just True ) vs
                when (not sat) $ reject $ vcat
                    [ text "clause" <+> toDoc cl <+> text "is not satisfied"
                    , text "values of literals are" <+> toDoc vs
                    ]
            return a
        Conflict cl -> do
            assert_no_conflict a
            let vs = map (\ l -> (l, evaluate_literal (assignment a) l)) cl
                unsat = all ( \ (l,v) -> v == Just False ) vs
            when (not unsat) $ reject $ vcat
                    [ text "clause" <+> toDoc cl <+> text "is not a conflict clause"
                    , text "values of literals are" <+> toDoc vs
                    ]
            return $ a { conflict_clause = Just cl }
        Backtrack -> assert_conflict a $ \ cl -> do
            let dl = decision_level a
            when (dl <= 0) $ reject 
                $ text "no previous decision (use Fail instead of Backtrack)"
            let [(v,i)] = filter (\(v,i) -> reason i == Decision && level i == dl) 
                        $ M.toList $ assignment a
                a' = reset_dl (pred dl) a
            if not (value i) 
               then work mo a' { conflict_clause = Nothing } ( Decide v ) -- positive
               else return a'
        Backjump { to_level = dl, learn = cl } -> do
            when (not $ clause_learning mo ) $ reject $ text "clause learning is not allowed"
            return $ undefined -- will not happen
        UNSAT -> assert_conflict a $ \ cl -> do
            let dl = decision_level a
            when (dl > 0) $ reject $ text "not at root decision level"
            return a
