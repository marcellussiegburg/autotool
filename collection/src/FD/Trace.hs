{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module FD.Trace where

import FD.Data

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Typeable

{-

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size
import qualified Autolib.Relation as R

import Data.List ( partition )
import Control.Applicative ((<$>))
import Data.List ( nub )
import Data.Maybe ( isNothing )

import Debug.Trace
-}

data Step u = Decide Var u
          | Arc_Cons { use :: (Atom, Var) , obtain :: S.Set u   }
          | Solved -- SAT
          | Failed (Maybe Var)
          | Backtrack           
          | Inconsistent -- UNSAT
    deriving (Eq, Typeable, Show)

steps :: [ Step Int ]
steps = [ Arc_Cons ( Atom (Rel "<") [Var "x", Var "y"] , Var "x") ( S.fromList[1..999])
        , Inconsistent
        ]

{-

instance Size Step where size = const 1

derives [makeReader] [''Step ]

instance ToDoc Step where toDoc = text . show

-}

data Reason = Decision 
            | Ultimate_Decision
            | Propagation 
    deriving (Eq, Typeable, Show)

-- derives [makeReader] [''Reason ]

-- instance ToDoc Reason where toDoc = text . show

data Info u = 
     Info { domain :: S.Set u
          , level :: Int
          , reason :: Reason
          }
    deriving (Eq, Typeable, Show)


data State u =
     State { decision_level :: Int
           , assignment :: M.Map Var (Info u)
           , failed :: Bool
           , variables :: S.Set Var
           , formula :: Formula
           , universe :: S.Set u
           }

state0 form u = State 
    { formula = form
    , universe = u
    , variables = S.fromList $ map variable $ concat cnf
    , decision_level = 0
    , assignment = M.empty
    , conflict_clause = Nothing
    }

{-

data Modus = Modus
     { require_complete_propagation :: Bool
     , decisions_must_be_negative :: Bool
     , clause_learning :: Bool
     , variable_elimination :: Bool
     }
    deriving (Eq, Typeable)

modus0 :: Modus
modus0 = Modus
     { require_complete_propagation = False
     , decisions_must_be_negative = True
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
            when (decisions_must_be_negative mo) $
                when p $ reject $ text "decision literal must be negative"
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
            when (clause_learning mo) $ reject $ text "Backtrack is not allowed (use Backjump)"
            let dl = decision_level a
            when (dl <= 0) $ reject 
                $ text "no previous decision (use UNSAT instead of Backtrack)"
            return $ backtrack a 
        Backjump { to_level = dl, learn = learnt_cl } -> assert_conflict a $ \ cl -> do
            when (not $ clause_learning mo ) $ reject $ text "clause learning is not allowed (use Backtrack)"
            reverse_unit_propagation_check a learnt_cl cl
            when (dl >= decision_level a) $ reject $ text "must jump to a lower level"
            return $ ( reset_dl dl a ) 
                { formula = learnt_cl : formula a, conflict_clause = Nothing }

        UNSAT -> assert_conflict a $ \ cl -> do
            let dl = decision_level a
            when (dl > 0) $ reject $ text "not at root decision level"
            return a

reverse_unit_propagation_check a lcl cl = do
    let antes = do 
            (v, Info { reason = Propagation { antecedent = a}}) <- M.toList $ assignment a
            return a
    ok <- is_implied_by lcl $ cl : antes
    when (not ok) $ reject 
       $ text "the learnt clause is not implied by conflict and antecedent clauses."
        
is_implied_by cl antes = 
    derive_conflict $ map ( \ l -> [ opposite l ] ) cl ++ antes

derive_conflict cnf = do
    let ( conf, noconf ) = partition null cnf
        ( unit, nounit ) = partition (\ cl -> length cl == 1 ) noconf
    if null conf 
        then if null unit
             then return False -- no conflict, no propagation
             else do
                let cnf' = foldr assign cnf $ concat unit
                derive_conflict cnf'
        else return True -- conflict

assign l cnf = 
    let ( sat, open) = partition ( \ cl -> elem l cl ) cnf
    in  map ( filter ( \ l' -> l' /= opposite l) ) open
    

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
            in  alternate  ( a' { conflict_clause = Nothing } ) v ( not $ value i ) 

conflict a cl = a { conflict_clause = Just cl }

alternate a v p = 
   let i = Info { value = p, level = decision_level a, reason = Alternate_Decision }
   in  a { assignment = M.insert v i $ assignment a }

must_be_unassigned a l = 
      maybe ( return () ) 
            ( const $ reject $ text "literal" <+> toDoc l <+> text "is already assigned")
  $ M.lookup (variable l) $ assignment a

assert_no_conflict a = 
      maybe ( return () )
            ( const $ reject $ text "must handle conflict first (by Backtrack, Backjump or UNSAT)" )
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

-}
