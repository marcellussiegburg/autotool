{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Completion.Improved where

import Rewriting.Completion.CP
import Rewriting.Completion.Simple (successors)
import qualified Rewriting.Termination as T
import qualified Rewriting.Termination.Interpretation  as T
import qualified Rewriting.Termination.Polynomial as P
import qualified Polynomial.Type as P

import Autolib.TES
import Autolib.TES.Identifier
import Autolib.TES.Unify 
import Autolib.TES.Apply

import Autolib.Reporter hiding ( run, execute )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Challenger.Partial
import Inter.Types

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
-- import Control.Applicative
import Data.Typeable

data Symbol c => Step v c 
    = Deduce { s :: Term v c, u :: Term v c, t :: Term v c }
    | Orient { s :: Term v c, t :: Term v c }
    | Delete { s :: Term v c }
    | Simplify { s :: Term v c, u :: Term v c, t :: Term v c }
    deriving ( Typeable, Eq )

data Symbol c => Problem c = 
     Problem { equations :: [(Term Int c, Term Int c)]
              , order_restriction :: T.Restriction 
              }
    deriving ( Typeable )

a x = Node ( mk 1 "a" ) [ x ]
b x = Node ( mk 1 "b" ) [ x ]
x = Autolib.TES.Var 1

problem0 = Problem
    { equations = [(a(b(a(x))), x)]
    , order_restriction = T.Polynomial
    }

data Symbol c => Run c = Run { order :: T.Order c
                 , steps :: [ Step Int c ]
                 }
    deriving ( Typeable )

run0 = Run
    { order = T.Interpretation 1  
            $ T.Polynomial_Interpretation
            $ M.fromList [ (mk 1 "a", 1 + P.variable (P.X 1))
                         , (mk 1 "b", 1 + P.variable (P.X 1))
                         ]
    , steps = [ Orient { s = a(b(a(x))), t = x } ]
    }

derives [makeReader, makeToDoc] [''Problem, ''Step, ''Run]

data Completion = Completion deriving Typeable

derives [makeReader, makeToDoc] [''Completion]

instance Show Completion where show = render . toDoc 

instance OrderScore Completion where
    scoringOrder _ = Increasing

instance Symbol c => Size (Run c) where size = length . steps

instance Partial Completion (Problem Identifier) (Run Identifier) where

    describe _ p = vcat 
        [ text "Give a rewrite order and a fair run of a completion procedure for the equations" 
          </> ( toDoc $ equations p )
        , text "The rewrite order must conform to"
          </> ( toDoc $ order_restriction p )
        ]

    initial _ p = run0

    partial _ p run = do
        let o = order run
            sig = S.fromList 
                $ do (l,r) <- equations p ; [l,r] >>= symsl
        T.check_dimensions sig o
        T.everything_monotone o
        ok <- T.compute_restriction (order_restriction p) o
        when (not ok) $ reject $ text "order does not conform to restriction"

    total _ p run = do
        let start = ( S.fromList $ equations p
                    , []
                    , S.fromList $ equations p
                    )
        (e, r, es) <- 
            foldM (execute $ order run) start $ steps run
        inform $ vcat
            [ text "equations" </> toDoc e
            , text "rules" </> toDoc r
            ]
        when (not $ S.null e) $ reject 
            $ text "set of equations is not empty"
        check_fairness r es

check_fairness r es = do
    forM_ (criticalpairs r) $ \ cp -> do
        let e = normalize $ expand cp
        when (not $ S.member e es) $ reject $ vcat
            [ text "run is not fair:"
            , text "critical pair" </> toDoc cp
            , text "with expansion" </> toDoc e
            , text "was never considered as an equation"
            ]

execute ord (e,r,es) step = do
    inform $ vcat
        [ text "equations" </> toDoc e
        , text "rules" </> toDoc r
        , text "step" </> toDoc step
        ]
    (e',r') <- case step of
        Deduce {} -> execute_deduce (e,r) step
        Orient {} -> execute_orient ord (e,r) step
        Delete {} -> execute_delete (e,r) step
        Simplify{}-> execute_simplify (e,r) step
    return (e', r', S.union e e')

execute_deduce (e,r) (Deduce{s=s,u=u,t=t}) = do
    let sus = successors r u
        info = text "R-successors of u are" </> toDoc sus
    when (not $ elem s sus) $ reject $ vcat
        [ text "s is not a R-successor of u.", info ]
    when (not $ elem t sus) $ reject $ vcat
        [ text "t is not a R-successor of u.", info ]
    return (S.union e $ S.singleton (s,t) , r )

execute_orient ord (e,r) (Orient{s=s,t=t}) = do
    let u = Rule { lhs = s, rhs = t, strict = True }
    o <- silent $ T.compute ord u
    when (o /= T.Greater) $ do
        inform $ text "Rule is not decreasing" </> toDoc u
        T.compute ord u
        reject $ empty
    return ( S.delete (s,t) $ S.delete (t,s) e
           , u : r
           )

execute_delete (e,r) (Delete{s=s}) = do
    return ( S.delete (s,s) e, r )

execute_simplify (e,r) (Simplify{s=s, u=u, t=t}) = do
    let ss = successors r s
    when ( not $ elem u ss ) $ reject $ vcat
        [ text "u is not an R-successor of s."
        , text "R-successors of s are" </> toDoc ss
        ]
    let ru = Rule { lhs = u, rhs = t, strict = True }
    return ( S.delete (s,t) $ S.delete (t,s) e
           , ru : r
           )

make_fixed :: Make
make_fixed = direct Completion problem0
