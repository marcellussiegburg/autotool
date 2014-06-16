{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module DPLL.Trace where

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
import Data.List ( nub )
import Data.Typeable
import Data.Maybe ( isNothing )

import Debug.Trace


data Step = Decide Literal
          | Propagate { use :: Clause, obtain :: Literal }
          | SAT
          | Conflict Clause
          | Backtrack 
          | Backjump { to_level :: Int, learn :: Clause }
          | UNSAT
    deriving (Eq, Typeable, Show)

instance Size Step where size = const 1

derives [makeReader] [''Step ]

instance ToDoc Step where toDoc = text . show

data Reason = Decision 
            | Propagation { antecedent :: Clause }
    deriving (Eq, Typeable, Show)

derives [makeReader] [''Reason ]

instance ToDoc Reason where toDoc = text . show

data Info = 
     Info { value :: Bool
          , level :: Int
          , reason :: Reason
          }
    deriving (Eq, Typeable, Show)

instance ToDoc Info where toDoc = text . show
derives [makeReader] [''Info]

data State =
     State { decision_level :: Int
           , assignment :: M.Map Variable Info
           , conflict_clause :: Maybe Clause
           , variables :: S.Set Variable
           , formula :: CNF
           }

state0 cnf = State 
    { formula = nub $ map nub cnf
    , variables = S.fromList $ map variable $ concat cnf
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

derives [makeReader, makeToDoc] [''State, ''Modus]


instance Show State where show = render . toDoc


execute :: Modus -> CNF -> [Step] -> Reporter State
execute mo cnf steps = foldM ( work mo ) (state0 cnf)  steps


work :: Modus -> State  -> Step -> Reporter State
work mo a s = do
    inform $ vcat [ text "current" </> toDoc a, text "step" </> toDoc s ]
    case s of
        Decide l -> do
            when (require_complete_propagation mo) $ assert_completely_propagated a
            assert_no_conflict a
            let p = positive l ; v = variable l
            must_be_unassigned a l
            when p $ reject $ text "initial decision must be negative"
            return $ decide a l
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
            return $ propagate a cl l
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
            return $ conflict a cl 
        Backtrack -> assert_conflict a $ \ cl -> do
            let dl = decision_level a
            when (dl <= 0) $ reject 
                $ text "no previous decision (use Fail instead of Backtrack)"
            return $ backtrack a 
        Backjump { to_level = dl, learn = cl } -> do
            when (not $ clause_learning mo ) $ reject $ text "clause learning is not allowed"
            return $ undefined -- will not happen
        UNSAT -> assert_conflict a $ \ cl -> do
            let dl = decision_level a
            when (dl > 0) $ reject $ text "not at root decision level"
            return a

decide a l = 
            let p = positive l ; v = variable l
                dl = succ $ decision_level a
            in  a 
                { decision_level = dl
                , assignment = M.insert v
                    (Info { value = p
                          , level = dl
                          , reason = Decision } ) $ assignment a
                }

propagate a cl l =    
            let dl = decision_level a
            in  a 
               { assignment = M.insert (variable l) (Info {
                   value = positive l, level = dl, reason = Propagation cl}) $ assignment a
               }

backtrack a = 
            let dl = decision_level a
                [(v,i)] = filter (\(v,i) -> reason i == Decision && level i == dl) 
                        $ M.toList $ assignment a
                a' = reset_dl (pred dl) a
            in  if not (value i) 
               then decide ( a' { conflict_clause = Nothing } ) (mkLiteral v True ) 
               else a' -- do not remove conflict_clause, need to backtrack more.

conflict a cl = a { conflict_clause = Just cl }

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

assert_completely_propagated a = do
    when (not $ null $ unit_clauses a) $ reject
        $ text "must do all unit propagations first"
    
is_unit_clause a cl = 
    let vs = map (evaluate_literal (assignment a)) cl
    in    ( not $ Just True `elem` vs )
       &&  ( 1 == length (filter isNothing vs) )

unit_clauses a = filter (is_unit_clause a) $ formula a

reset_dl dl a = a 
    { assignment = M.filter ( \ i -> level i <= dl ) $ assignment a
    , decision_level = dl
    }

evaluate_literal a l = do
    let f = if positive l then id else not
    f <$> value <$> M.lookup (variable l) a

