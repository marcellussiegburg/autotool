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
import qualified Data.Set as S

data Restriction = 
       And [ Restriction ]
     -- | Or Restriction | Not Restriction
     | No_Lexicographic_Combination
     | Allow_Matrix_Natural
     | Allow_Matrix_Arctic
     | Allow_Matrix_Tropical
     | Allow_Matrix_Fuzzy
     | Max_Dimension Int
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Restriction]

data Symbol c => Problem c = 
     Problem { system :: TRS c c
             , restriction :: Restriction
             }
    deriving ( Eq, Typeable )

problem0 :: Problem Identifier
problem0 = Problem 
    { system = read "TRS { variables = [x, y] , rules = [ f(x,y) -> f(y,x) ] }"
    , restriction = And [ No_Lexicographic_Combination ]
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
      Lexicographic ords -> succ $ sum $ map size ords

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
        ]
    initial _ p = Interpretation 2 
        $ Matrix_Interpretation_Natural
        $ M.fromList 
        $ zip (S.toList $ signature $ system p)
        $ repeat $ projection 3 2 2
    partial _ p o = do
        everything_monotone o
        check_restriction (restriction p) o
    total _ p o = do
        compatible o $ system p

make_fixed :: Make
make_fixed = direct Rewriting_Termination problem0

check_restriction r o = case r of
    And rs -> forM_ rs $ \ r -> check_restriction r o
    No_Lexicographic_Combination -> case o of
        Lexicographic {} -> reject $ text "forbidden"
        _ -> return ()
    _ -> return () -- FIXME

everything_monotone o = case o of
    Empty -> return ()
    Interpretation {} -> check_monotone $ values o
    Lexicographic ords -> forM_ ords everything_monotone


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
