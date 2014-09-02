{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# language DatatypeContexts #-}
{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}

module Rewriting.Termination where

import Rewriting.Termination.Semiring
import Rewriting.Termination.Multilinear
import Rewriting.Termination.Interpretation
import Rewriting.TRS

import Challenger.Partial
import Inter.Types

import Autolib.Symbol
import Autolib.Size
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.FiniteMap
import qualified Data.Map as M
import Data.Typeable
import Control.Monad ( when, forM )
import Control.Applicative ( (<$>) )
import qualified Data.Set as S

data Restriction = 
       And [ Restriction ] | Or [ Restriction ] | Not Restriction
     | Lexicographic_with_children Restriction
     | Matrix_Natural
     | Matrix_Fuzzy
     | Matrix_Arctic
     | Matrix_Tropical
     | Matrix_dimension_at_most Int
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Restriction]

data Symbol c => Problem c = 
     Problem { system :: TRS c c
             , restriction :: Restriction
             }
    deriving ( Eq, Typeable )

problem0 :: Problem Identifier
problem0 = Problem 
    { system = read "TRS { variables = [x] , rules = [ a(b(x)) -> b(a(x)) ] }"
    , restriction = Or [ Matrix_Natural, Lexicographic_with_children Matrix_Natural ]
    }

derives [makeReader, makeToDoc] [''Problem]

data Symbol c => Order c 
    = Empty
    | Interpretation { dim :: Int, values :: Interpretation c }
    | Lexicographic [ Order c ]
    deriving ( Eq, Typeable )

instance Symbol c => Size (Order c) where
   size o = case o of
      Empty -> 0
      Interpretation {} -> size $ values o
      Lexicographic ords -> sum $ map size ords

derives [makeReader, makeToDoc] [''Order]

data Rewriting_Termination = Rewriting_Termination
    deriving Typeable

derives [makeReader, makeToDoc] [''Rewriting_Termination]

instance Show Rewriting_Termination where show = render . toDoc

instance OrderScore Rewriting_Termination where
    scoringOrder _ = Increasing

instance Symbol c => Partial Rewriting_Termination (Problem c) (Order c) where
    describe _ p = vcat
        [ text "give a rewrite order"
        , text "that is compatible with" <+> toDoc (system p)
        , text "and conforms to" <+> toDoc (restriction p)
        , text "--"
        , text "matrix syntax: scalar 3, row [4,5,6], column [1,2,3], matrix [[1,2],[3,4]], unit 3, zero (2,1)"
        , text "semiring elements: -inf, 0, 1, .. , +inf"
        , text "domains: _Natural, _Arctic, _Tropical, _Fuzzy"
        , text "size (for highscore): total number of nonzero entries"
        ]
    initial _ p = Interpretation 2 
        $ Matrix_Interpretation_Natural
        $ M.fromList $ do 
            (k,f) <- zip [1..] $ S.toList $ signature $ system p
            return ( f, projection (arity f) (succ $ k `mod` arity f) 2 )
    partial _ p o = do
        check_dimensions (signature $ system p) o
        everything_monotone o
        ok <- compute_restriction (restriction p) o
        when (not ok) $ reject $ text "order does not conform to restriction"
    total _ p o = do
        compatible o $ system p

make_fixed :: Make
make_fixed = direct Rewriting_Termination problem0

compute_restriction r o = case r of
    And rs -> and <$> forM rs ( \ r -> compute_restriction r o )
    Or  rs ->  or <$> forM rs ( \ r -> compute_restriction r o )
    Not r  -> not <$> compute_restriction r o
    _ -> case (r,o) of
        ( Lexicographic_with_children r, Lexicographic ords ) -> 
            and <$> forM ords ( \ o -> compute_restriction r o )
        ( Matrix_Natural, Interpretation {values = Matrix_Interpretation_Natural {}} ) -> return True 
        ( Matrix_Arctic, Interpretation {values = Matrix_Interpretation_Arctic {}}) -> return True 
        ( Matrix_Tropical, Interpretation { values = Matrix_Interpretation_Tropical {}} ) -> return True 
        ( Matrix_Fuzzy, Interpretation { values = Matrix_Interpretation_Fuzzy {}} ) -> return True 
        ( Matrix_dimension_at_most d, Interpretation { dim = dim} ) -> return $ d >= dim
        _ -> return False
    
everything_monotone o = case o of
    Empty -> return ()
    Interpretation {} -> check_monotone $ values o
    Lexicographic ords -> forM_ ords everything_monotone

check_dimensions sig o = case o of
    Empty -> return ()
    Lexicographic ords -> forM_ ords $ check_dimensions sig
    Interpretation {} ->  check_dimension sig (dim o) (values o)
        


signature sys = S.unions $ do
    u <- rules sys ; [ syms $ lhs u, syms $ rhs u ]


compatible ord sys = forM_ (rules sys) $ \ u -> do
    o <- compute ord u
    when (o /= Greater) $ reject $ text "must be greater"

compute ord u = case ord of
    Empty -> return Greater_Equal
    Interpretation {dim=d, values=int} -> compute_order int d u
    Lexicographic ords -> compute_lex ords u
        
compute_lex [] u = return Greater_Equal
compute_lex (ord:ords) u = do
    c <- compute ord u
    case c of
        Greater -> return Greater
        Greater_Equal -> compute_lex ords u
        Other -> return Other
