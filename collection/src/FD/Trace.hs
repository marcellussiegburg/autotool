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
import Data.List ( nub, (\\) )
import Data.Maybe ( isNothing, isJust )


data Step u = Decide Var u
          | Arc_Consistency_Deduction { atom :: Atom, variable :: Var, restrict_to :: [ u ]   }
          | Solved -- SAT
          | Empty_Domain Var
          | Backtrack 
          | Inconsistent -- UNSAT
    deriving Typeable

steps0 :: [ Step Int ]
steps0 = [ Arc_Consistency_Deduction 
            ( Atom (Rel "P") [Var "x", Var "x", Var "y"]) ( Var "x") [ 1 .. 2 ]
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
          , decision :: Bool 
          , domain :: [ u ]
          , future_domain :: [ u ]
          }
    deriving ( Typeable, Show )

derives [makeReader] [''Info ]
instance ToDoc u => ToDoc (Info u) where 
    toDoc i = 
        let t = text
        in  oneline $ named_dutch_record (t "Info")
            [ t "level =" <+> toDoc (level i)
            , t "decision =" <+> toDoc (decision i)
            , t "domain =" <+> toDoc (domain i)
            , t "future_domain =" <+> toDoc (future_domain i)
            ]

-- FIXME this is super-ugly:
oneline = text . unwords . words . render

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
           , domain_assignment :: M.Map Var (Info u)
           , failed :: Bool
           }

derives [makeReader, makeToDoc] [''State ]

state0 inst = State 
    { decision_level = 0
    , domain_assignment = M.fromList $ do
          v <- S.toList $ variables $ formula inst
          return (v, Info { domain = universe $ algebra inst
                          , level = 0, future_domain = [], decision = False } )
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
        Empty_Domain var -> work_empty_domain inst a var
        Backtrack -> work_backtrack inst a 
        Inconsistent -> work_inconsistent inst a
        Solved -> work_solved inst a
        Arc_Consistency_Deduction {} -> work_arc_consistency_deduction inst a s

work_arc_consistency_deduction inst a s = do
    let at = atom s ; var = variable s ; res = restrict_to s
    when ( not $ elem at $ formula inst ) $ reject
        $ text "atom does not occur in formula"
    let vs = variables [ at ]
    when ( not $ S.member var vs ) $ reject 
        $ text "variable does not occur in atom"
    case allow_hyperarc_propagation_up_to $ modus inst  of
        Nothing -> return ()
        Just bound -> do
            let free v = (> 1) $ length $ domain $ domain_assignment a M.! v 
                free_vars = S.filter free vs
                f = S.size free_vars
            when (f > bound) $ reject $ vcat
                [  text "this atom contains" <+> toDoc f <+> text "variables with non-unit domain:" </> toDoc free_vars
                , text "but deduction is only allowed for hyper-edges of size up to" <+> toDoc bound
                ]
    info <- get_info a var
    case res \\ domain info of
        [] -> return ()
        bad -> reject $ text "these elements are not in the domain" <+> toDoc bad
    let wrong = do
            elt <- domain info \\ res
            m <- take 1 $ do
                    b0 <- assignments a $ S.delete var vs
                    let b = M.insert var elt b0
                    guard $ evaluate (algebra inst) [ at ] b
                    return b
            return ( elt, m )
    when (not $ null wrong) $ reject $ vcat
        [ text "these elements cannot be excluded from the domain of the variable,"
        , text "because the given assignment is a model for the atom:"
        , toDoc wrong
        ]
    let info' = info { domain = res }
    return $ a { domain_assignment = M.insert var info' $ domain_assignment a }
    
assignments a vs = map M.fromList 
    $ sequence  
    $ map ( \ (v, i) -> map ( \ e -> (v,e)) (domain i)) 
    $ M.toList 
    $ M.filterWithKey ( \ v i -> S.member v vs ) 
    $ domain_assignment a 

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
    let these = M.filter ( \ i -> level i == dl ) $ domain_assignment a
        previous = M.filter ( \ i -> level i < dl ) $ domain_assignment a
        [ (var, info ) ] = M.toList $ M.filter decision these
        info' = Info { level = dl' , decision = False
                     , domain = future_domain info, future_domain = [] }
    return $ a { decision_level = dl'
               , domain_assignment = M.insert var info' $ domain_assignment a 
               , failed = False
               }

work_empty_domain inst a var = do
    assert_not_failed a
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
    let todo = filter ( /= elt ) dom ++ future_domain info
        info' = if null todo
            then Info { level = decision_level a
                    , domain = [elt]
                    , future_domain = []
                    , decision = False
                    }
            else Info { level = succ $ decision_level a
                      , domain = [elt]
                      , future_domain = todo
                      , decision = True
                      }
    return $ State 
                 { decision_level = level info'
                 , failed = False
                 , domain_assignment = M.insert var info' $ domain_assignment a
                 }

get_info a var = 
    maybe ( reject $ text "unbound variable" ) return
         $ M.lookup var ( domain_assignment a )
    
assert_hyperarc_consistent inst a = 
    case require_hyperarc_consistency_up_to $ modus $ inst of
        Nothing -> return ()
        Just bound -> do
            reject $ text "assert_hyperarc_consistent: not implemented"

