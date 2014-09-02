{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
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
          | Arc_Consistency_Deduction 
              { atoms :: [ Atom ], variable :: Var, restrict_to :: [ u ]   }
          | Solved -- SAT
          | Backtrack 
          | Inconsistent -- UNSAT
    deriving Typeable

steps0 :: [ Step Int ]
steps0 = [ Arc_Consistency_Deduction 
            [ Atom (Rel "P") [Var "x", Var "x", Var "y"] ] ( Var "x") [ 1 .. 2 ]
        , Inconsistent
        ]

instance Size (Step u) where size = const 1

derives [makeReader, makeToDoc] [''Step ]


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
    , allow_hyperarc_propagation_up_to = Just 1
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

instance0 :: Instance Int
instance0 = Instance 
    { modus = modus0
    , algebra = algebra0
    , formula = formula0
    }

instance1 :: Instance Int
instance1 = Instance
    { modus = Modus
          { require_hyperarc_consistency_up_to = Nothing
          , allow_hyperarc_propagation_up_to = Just
                1
          , decisions_must_be_increasing = True
          , max_solution_length = Nothing 
          }
    , formula = read "[ G(x,y), G(y,z),G(z,x) ]"
    , algebra = Algebra
          { universe = [ 0 , 1 , 2 , 3
                       ]
          , relations = M.fromList
                [ ( read "G"
                  , S.fromList
                        [ [ 1 , 0 ] , [ 2 , 0 ]
                        , [ 2 , 1 ] , [ 3 , 0 ]
                        , [ 3 , 1 ] , [ 3 , 2 ] ] )
                 ] 
          } 
    }


-- | a stack: 
-- always non-empty,
-- decision level is length of stack
-- current state is in front,
-- if some domain is empty, then it's failed
data State u = Stack [ M.Map Var [u] ]

current (Stack xs) = head xs
history (Stack xs) = Stack (tail xs)
replace x (Stack xs) = Stack (x : tail xs)
push x (Stack xs) = Stack (x : xs)
at_top (Stack [x]) = True ; at_top _ = False

failed s = any null $ M.elems $ current s



derives [makeReader, makeToDoc] [''State ]

state0 inst = Stack 
    [ M.fromList $ do
          v <- S.toList $ variables $ formula inst
          return (v, universe $ algebra inst )
    ]


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
        Backtrack -> work_backtrack inst a 
        Inconsistent -> work_inconsistent inst a
        Solved -> work_solved inst a
        Arc_Consistency_Deduction {} -> work_arc_consistency_deduction inst a s

work_arc_consistency_deduction inst a s = do
    let var = variable s ; res = restrict_to s
    let bad_atoms = atoms s \\ formula inst
    when (not $ null bad_atoms) $ reject $ 
        text "these atoms do not occur in the formula:" </> toDoc bad_atoms
    let vs = variables $ atoms s
    when ( not $ S.member var vs ) $ reject 
        $ text "variable does not occur in atoms"
    case allow_hyperarc_propagation_up_to $ modus inst  of
        Nothing -> return ()
        Just bound -> do
            let fvs = free_vars a $ atoms s
                f = S.size fvs
            when (f > bound) $ reject $ vcat
                [  text "these atom contain" <+> toDoc f <+> text "variables with non-unit domain:" </> toDoc fvs
                , text "but deduction is only allowed for hyper-edges of size up to" <+> toDoc bound
                ]
    dom <- get_domain a var
    case res \\ dom of
        [] -> return ()
        bad -> reject $ text "these elements are not in the domain" <+> toDoc bad
    let wrong = do
            elt <- dom \\ res
            m <- take 1 $ satisfying_assignments inst a (atoms s) var elt
            return ( elt, m )
    when (not $ null wrong) $ reject $ vcat
        [ text "these elements cannot be excluded from the domain of the variable,"
        , text "because the given assignment is a model for the atoms:"
        , toDoc wrong
        ]
    return $ replace (M.insert var res $ current a) a

satisfying_assignments inst a f var elt = do
    let vs = variables f
    b0 <- assignments a $ S.delete var vs
    let b = M.insert var elt b0
    guard $ evaluate (algebra inst) f b
    return b
    
assignments a vs = map M.fromList 
    $ sequence  
    $ map ( \ (v, dom) -> map ( \ d -> (v,d)) dom )
    $ M.toList 
    $ M.filterWithKey ( \ v i -> S.member v vs ) 
    $ current a 

free a v = (> 1) $ length $ current a M.! v 
free_vars a f = S.filter (free a) $ variables f
                
evaluate alg form b = and $ map (evaluate_atom alg b) form

evaluate_atom alg b atom = 
    let argv = map ( b M.! ) $ args atom
    in  S.member argv $ relations alg M.! rel atom

assert_failed a = 
    when (not $ failed a) $ reject $ text "only allowed in a failed state"

assert_not_failed a = 
    when (failed a) $ reject $ text "only allowed in a non-failed state"

is_solved inst a = 
    let bs = assignments a $ variables $ formula inst
        (ok, wrong) = partition (evaluate ( algebra inst) ( formula inst)) bs
    in  null wrong && not (null ok)

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
    when ( not $ at_top a ) $ reject 
        $ text "Inconsistency can only be claimed at top level (use Backtrack)"
    return a

work_backtrack inst a = do
    assert_failed a
    when ( at_top a ) $ reject $ text "cannot backtrack at top level (use Inconsistent)"
    return $ history a

work_decide inst a var elt = do
    assert_not_failed a
    assert_hyperarc_consistent inst a
    dom <- get_domain a var
    when (length dom < 2) $ reject $ text "decision must be about a domain of size >= 2"
    when (not $ elem elt dom) $ reject 
         $ text "not in current domain of the variable"
    when ( decisions_must_be_increasing $ modus inst ) $ do
         when (not ( elt == minimum dom )) $ reject 
             $ text "is not the minimum element of the domain"
    let todo = filter ( /= elt ) dom
    return $ push (M.insert var [elt] $ current a)
           $ replace ( M.insert var todo $ current a ) -- tail call opt
           $ a

get_domain a var = 
    maybe ( reject $ text "unbound variable" ) return
         $ M.lookup var ( current a )
    
assert_hyperarc_consistent inst a = 
    case require_hyperarc_consistency_up_to $ modus $ inst of
        Nothing -> return ()
        Just bound -> do
            reject $ text "assert_hyperarc_consistent: not implemented"

