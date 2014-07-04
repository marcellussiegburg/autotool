{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language DatatypeContexts #-}

module FD.Trace where

import FD.Data

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Typeable

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size
import qualified Autolib.Relation as R

import Data.List ( partition )
import Control.Applicative ((<$>))
import Data.List ( nub )
import Data.Maybe ( isNothing )


data Step u = Decide Var u
          | Arc_Consistency_Deduction { use :: (Atom, Var) , obtain :: [ u ]   }
          | Solved -- SAT
          | Failed (Maybe Var)
          | Backtrack           
          | Inconsistent -- UNSAT
    deriving Typeable

steps :: [ Step Int ]
steps = [ Arc_Consistency_Deduction 
            ( Atom (Rel "<") [Var "x", Var "y"] , Var "x") [ 1 .. 999 ]
        , Inconsistent
        ]

instance Size (Step u) where size = const 1

derives [makeReader, makeToDoc] [''Step ]


data Reason = Initial
            | Decision 
            | Ultimate_Decision -- ^ skip on backtrack
            | Propagation 
    deriving ( Typeable, Show )

derives [makeReader, makeToDoc] [''Reason ]


data Info u = 
     Info { level :: Int
          , domain :: [ u ]
          , future_domain :: Maybe [u] 
            -- ^ for decision nodes: the branches that need to be taken in the future
          }
    deriving Typeable

derives [makeReader, makeToDoc] [''Info ]

data Ord u => Algebra u = 
     Algebra { universe :: [u]
             , relations :: M.Map Rel ( S.Set [u] )
             }
    deriving Typeable

derives [makeReader, makeToDoc] [''Algebra ]

data Modus = Modus
     { require_hyperarc_consistency_up_to :: Maybe Int
     , allow_hyperarc_propagation_up_to :: Maybe Int
     , decisions_must_be_increasing :: Bool
     }
    deriving (Eq, Typeable)

derives [makeReader, makeToDoc] [''Modus]

modus0 :: Modus
modus0 = Modus
    { require_hyperarc_consistency_up_to = Nothing
    , allow_hyperarc_propagation_up_to = Just 2
    , decisions_must_be_increasing = True
    }

data Ord u => Instance u = 
     Instance { modus :: Modus
              , formula :: Formula
              , algebra :: Algebra u
              }

derives [makeReader, makeToDoc] [''Instance]

data State u =
     State { decision_level :: Int
           , assignment :: M.Map Var (Info u)
           , failed :: Bool
           }

derives [makeReader, makeToDoc] [''State ]

state0 inst = State 
    { decision_level = 0
    , assignment = M.fromList $ do
          v <- S.toList $ variables $ formula inst
          return (v, Info { domain = universe $ algebra inst
                          , level = 0, future_domain = Nothing } )
    , failed = False
    }


instance ToDoc u => Show (State u) where show = render . toDoc


execute :: (Ord u, ToDoc u) 
        => Instance u -> [Step u] -> Reporter (State u)
execute inst steps = foldM (work inst) (state0 inst)  steps


work :: (Ord u, ToDoc u) 
     => Instance u -> State u  -> Step u -> Reporter (State u)
work inst a s = do
    inform $ vcat [ text "current" </> toDoc a, text "step" </> toDoc s ]

    case s of
        Decide var elt -> do
            case require_hyperarc_consistency_up_to $ modus $ inst of
                Just bound -> assert_hyperarc_consistent bound inst a
                Nothing -> return ()
            info <- maybe ( reject $ text "unbound variable" ) return
                 $ M.lookup var ( assignment a )
            let dom = domain info
            when (not $ elem elt dom) $ reject 
                 $ text "not in current domain of the variable"
            when ( decisions_must_be_increasing $ modus inst ) $ do
                 when (not ( elt == minimum dom )) $ reject 
                     $ text "is not the minimum element of the domain"
            let todo = filter ( /= elt ) dom
                info = Info { level = succ $ decision_level a
                            , domain = [elt]
                            , future_domain = Just todo
                            }
            return $ State 
                   { decision_level = succ $ decision_level a
                   , failed = False
                   , assignment = M.insert var info $ assignment a
                   }

{-
            
          | Arc_Consistency_Deduction { use :: (Atom, Var) , obtain :: [ u ]   }
          | Solved -- SAT
          | Failed (Maybe Var)
          | Backtrack           
          | Inconsistent -- UNSAT


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
-}

assert_hyperarc_consistent bound form a = do
    reject $ text "assert_hyperarc_consistent: not implemented"

{-
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
