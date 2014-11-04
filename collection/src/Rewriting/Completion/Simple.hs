{-# language DoAndIfThenElse #-}

module Rewriting.Completion.Simple where

import Rewriting.Completion.CP
import Rewriting.Termination
import Rewriting.Termination.Interpretation 
import qualified Rewriting.Termination.Polynomial as P
import qualified Polynomial.Type as P

import Autolib.TES
import Autolib.TES.Identifier
import Autolib.TES.Unify 
import Autolib.TES.Apply

import Autolib.Reporter hiding ( run )
import Autolib.ToDoc

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import Data.List ( nub )

check1 = 
    let a x = Node ( mk 1 "a" ) [ x ]
        b x = Node ( mk 1 "b" ) [ x ]
        x = Var (mk 0 "x")
        ord = Interpretation 1  
            $ Polynomial_Interpretation
            $ M.fromList [ (mk 1 "a", 1 + P.variable (P.X 1))
                         , (mk 1 "b", 2 * P.variable (P.X 1))
                         ]
    in  complete [ ( a(b(a(x))), x ) ]   ord 10


check3 = 
    let a x = Node ( mk 1 "a" ) [ x ]
        b x = Node ( mk 1 "b" ) [ x ]
        x = Var (mk 0 "x")
        ord = Interpretation 1  
            $ Polynomial_Interpretation
            $ M.fromList [ (mk 1 "a", 1 + P.variable (P.X 1))
                         , (mk 1 "b", 2 * P.variable (P.X 1))
                         ]
    in  complete [ ( a(b(a(x))), b(x) ) ]   ord 10

check2 = 
    let f x y = Node ( mk 2 "f" ) [ x,y ]
        x = Var (mk 0 "x")
        y = Var (mk 0 "y")
        z = Var (mk 0 "z")
        ord = Interpretation 1  
            $ Polynomial_Interpretation
            $ M.fromList [ (mk 2 "f", 1 + P.variable (P.X 1) + P.variable (P.X 2) ) ]
    in  complete [ ( f(f x y) (f y z), y) ]   ord 1

check4 = 
    let f x y = Node ( mk 2 "f" ) [ x,y ]
        i x = Node ( mk 1 "i") [x]
        e = Node (mk 0 "e") []
        x = Var (mk 0 "x")
        y = Var (mk 0 "y")
        z = Var (mk 0 "z")
        v k = P.variable (P.X k)
        ord = Interpretation 1  
            $ Polynomial_Interpretation
            $ M.fromList 
            [ (mk 2 "f", 1 + v 1 + 3 * v 2 )
            , (mk 0 "e", 0 )
            , (mk 1 "i", 1 + v 1)
            ]
    in  complete [ ( f x (f  y z) , f (f x y) z )
                 , ( f e x, x )
                 , ( f (i x) x, e )
                 ]   ord 3

complete :: (Ord v, Symbol c)
         => [(Term v c, Term v c)]
         -> Order c
         -> Int
         -> Reporter [Rule (Term Int c)]
complete eqs ord steps = do
    inform $ text "compute initial rule set"
    start <- forM ( filter ( not . uncurry (==)) eqs )
             $ \ eq -> orient ord $ normalize eq 
    inform $ text "add resolutions of critical pairs"
    run ord 0 steps $ S.fromList start 

run :: (Symbol c)
    => Order c -> Int -> Int 
    -> S.Set (Rule(Term Int c))
    -> Reporter [Rule (Term Int c)]
run ord step bound rules = do
    inform $ vcat [ text "step" </> toDoc step
                  , text "rules" </> toDoc rules 
                  ]
    when (step > bound) $ reject $ text "too many steps"
    next <- forM (criticalpairs $ S.toList rules) $ \ cp -> do
        let (l,r) = normalize $ expand cp
        inform $ vcat [ text "critical pair" </> toDoc cp
                      , text "expands to" </> toDoc (l,r)
                      ]
        let nls = normalforms (S.toList rules) l
            nrs = normalforms (S.toList rules) r
        inform $ vcat
            [ text "normal forms of lhs" </> toDoc nls
            , text "normal forms of rhs" </> toDoc nrs
            ]
        let equal = or $ do 
                s <- nls ; t <- nrs
                return $ s == t
        let oriented = do
                s <- nls ; t <- nrs
                let mo = result $ orient ord (s,t)
                maybeToList mo
        if equal then do
            inform $ text "equal pair among lhs and rhs"
            return [] 
        else case oriented of
            []  -> reject $ text "no pair of normal forms can be ordered"
            o : _ -> do
                inform $ vcat [ text "pick oriented pair" </> toDoc o ]
                return [o]
    let rules' = S.union rules $ S.fromList $ concat next
    if  rules' /= rules
    then run ord (step + 1) bound rules'
    else do
        inform $ text "obtained convergent system"
               </> toDoc rules
        return $ S.toList rules

orient :: (Ord v, Symbol c, ToDoc v)
       => Order c -> (Term v c, Term v c)
       -> Reporter (Rule (Term v c))
orient ord (l,r) = do
    o <- silent $ compute ord $ Rule { lhs = l, rhs = r }
    if o == Greater then return $ Rule {lhs=l,rhs=r,strict=True}
    else do
        o <- silent $ compute ord $ Rule { lhs = r, rhs = l }
        if o == Greater then return $ Rule{lhs=r,rhs=l,strict=True}
        else reject $ vcat
                [ text "pair cannot be ordered:"
                , nest 4 $ toDoc (l,r)
                ]

-- | one step
successors :: (Ord u, Ord v, Eq c)
           => [ Rule(Term v c)] 
            -> Term u c 
            -> [ Term u c ]
successors rules t = do
    (p, sub) <- positions t
    rule <- rules
    u <- maybeToList $ match (lhs rule) sub
    return $ poke t (p, apply u $ rhs rule)


-- | rules must be terminating (this is not checked here)
descendants :: (Ord u, Ord v, Eq c)
            => [Rule (Term v c)] 
            -> Term u c 
            -> [ Term u c ]
descendants rules t = nub $
    t : (successors rules t >>= descendants rules)

-- | rules must be terminating (this is not checked here)
normalforms :: (Ord u, Ord v, Eq c)
            => [Rule (Term v c)] 
            -> Term u c 
            -> [ Term u c ]
normalforms rules t = 
    case successors rules t of
        [] -> [t]
        ts -> ts >>= normalforms rules
