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
import Data.Maybe ( isNothing, isJust )


data Step u = Decide Var u
          | Arc_Consistency_Deduction { use :: (Atom, Var) , obtain :: [ u ]   }
          | Solved -- SAT
          | Failed (Maybe Var)
          | Backtrack 
          | Inconsistent -- UNSAT
    deriving Typeable

steps0 :: [ Step Int ]
steps0 = [ Arc_Consistency_Deduction 
            ( Atom (Rel "<") [Var "x", Var "y"] , Var "x") [ 1 .. 3 ]
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
instance (ToDoc u, Ord u) => Show ( Algebra u) where show = render . toDoc

algebra0 :: Algebra Int
algebra0 = 
    let u = [ 0 .. 3 ] 
    in Algebra
        { universe = u
        , relations = M.fromList 
             [ ( Rel "G", S.fromList [ [x,y] | x<-u, y<-u, x >y ] )
             , ( Rel "P", S.fromList [ [x,y,z] | x<-u, y<-u, z<-u, x + y == z ] )
             ]
       }

data Modus = Modus
     { require_hyperarc_consistency_up_to :: Maybe Int
     , allow_hyperarc_propagation_up_to :: Maybe Int
     , decisions_must_be_increasing :: Bool
     , max_solution_length :: Maybe Int
     }
    deriving (Eq, Typeable)

derives [makeReader, makeToDoc] [''Modus]

modus0 :: Modus
modus0 = Modus
    { require_hyperarc_consistency_up_to = Nothing
    , allow_hyperarc_propagation_up_to = Just 2
    , decisions_must_be_increasing = True
    , max_solution_length = Nothing
    }

data Ord u => Instance u = 
     Instance { modus :: Modus
              , formula :: Formula
              , algebra :: Algebra u
              }
    deriving (Typeable)

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
        Decide var elt -> work_decide inst a var elt
        Failed mvar -> work_failed inst a mvar
        Backtrack -> work_backtrack inst a 
        Inconsistent -> work_inconsistent inst a
        Solved -> work_solved inst a
        Arc_Consistency_Deduction {} -> work_arc_consistency_deduction inst a s

work_arc_consistency_deduction inst a 
        (Arc_Consistency_Deduction { use = (atom, var), obtain = elts }) = do
    let vs = variables [ atom ]
    when ( not $ S.member var vs ) $ reject 
        $ text "variable does not occur in the atom"
    case allow_hyperarc_propagation_up_to $ modus inst  of
        Nothing -> return ()
        Just bound -> when (S.size vs > bound) $ reject 
            $ text "deduction for hyper-edges of size" <+> toDoc (S.size vs) <+> text "is not allowed"
    info <- get_info a var
    let ok = and $ do
            elt <- elts
            return $ or $ do
                    b0 <- assignments a $ S.delete var vs
                    let b = M.insert var elt b0
                    return $ evaluate (algebra inst) [atom] b
    when (not ok) $ reject 
        $ text "not all elements of the target domain have a matching assignment"
    let info' = info { domain = elts }
    return $ a { assignment = M.insert var info' $ assignment a }
    
assignments a vs = map M.fromList 
    $ sequence  
    $ map ( \ (v, i) -> map ( \ e -> (v,e)) (domain i)) 
    $ M.toList $ assignment a 

evaluate alg form b = and $ map (evaluate_atom alg b) form

evaluate_atom alg b atom = 
    let argv = map ( b M.! ) $ args atom
    in  S.member argv $ relations alg M.! rel atom

assert_failed a = 
    when (not $ failed a) $ reject $ text "only allowed in a failed state"

assert_not_failed a = 
    when (failed a) $ reject $ text "only allowed in a non-failed state"

work_solved inst a = do
    assert_not_failed a
    let bs = assignments a $ variables $ formula inst
    let (ok, wrong) = partition (evaluate ( algebra inst) ( formula inst)) bs
    when (not $ null wrong) $ reject $ vcat
        [ text "there is some assignment that is a non-model"
        , toDoc $ take 1 $ wrong
        ]
    when (null ok) $ reject $ text "there is no assignment (some domain is empty)"
    return a

work_inconsistent inst a = do
    assert_failed a
    let dl = decision_level a ; dl' = pred dl
    when ( dl > 0 ) $ reject 
        $ text "Inconsistency can only be claimed at top level (use Backtrack)"
    return a

work_backtrack inst a = do
    assert_failed a
    let dl = decision_level a ; dl' = pred dl
    when ( dl <= 0 ) $ reject $ text "cannot backtrack at level 0 (use Inconsistent)"
    let these = M.filter ( \ i -> level i == dl ) $ assignment a
        previous = M.filter ( \ i -> level i < dl ) $ assignment a
        [ (var, Info { future_domain = Just fdom } ) ] 
            = M.toList $ M.filter ( isJust . future_domain ) these
        info' = Info { level = dl', domain = fdom, future_domain = Nothing }
    return $ a { decision_level = dl'
               , assignment = M.insert var info' $ assignment a 
               , failed = False
               }

work_failed inst a mvar = do
    assert_not_failed a
    case mvar of
      Nothing -> do
        if S.null $ variables $ formula inst
            then error "work_failed { mvar = Nothing } not implemented"
            else reject $ text "Failed Nothing can only be used when there are no variables"
      Just var -> do
        info <- get_info a var
        if null $ domain info
            then return $ a { failed = True }
            else reject $ text "domain of this variable is not empty"

work_decide inst a var elt = do
    assert_not_failed a
    assert_hyperarc_consistent inst a
    info <- get_info a var
    let dom = domain info
    when (not $ elem elt dom) $ reject 
         $ text "not in current domain of the variable"
    when ( decisions_must_be_increasing $ modus inst ) $ do
         when (not ( elt == minimum dom )) $ reject 
             $ text "is not the minimum element of the domain"
    let todo = filter ( /= elt ) dom
        info' = if null todo
            then Info { level = decision_level a
                    , domain = [elt]
                    , future_domain = Nothing
                    }
            else Info { level = succ $ decision_level a
                      , domain = [elt]
                      , future_domain = Just todo
                      }
    return $ State 
                 { decision_level = level info'
                 , failed = False
                 , assignment = M.insert var info' $ assignment a
                 }

get_info a var = 
    maybe ( reject $ text "unbound variable" ) return
         $ M.lookup var ( assignment a )
    
assert_hyperarc_consistent inst a = 
    case require_hyperarc_consistency_up_to $ modus $ inst of
        Nothing -> return ()
        Just bound -> do
            reject $ text "assert_hyperarc_consistent: not implemented"

