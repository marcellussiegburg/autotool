{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Fixed where

import Rewriting.Abstract.Syntax
import Rewriting.Abstract.Semantics
import Rewriting.Abstract.Braced


import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import Autolib.TES
import Autolib.TES.Identifier
import qualified Autolib.Relation as R

import Autolib.FiniteMap
import qualified Data.Map as M
import qualified Data.Set as S

import Challenger.Partial
import Inter.Types
import Data.Typeable
import Control.Monad ( when )

data Problem = 
     Problem { property :: Prop
             , domain_size_should_be :: (Ordering, Int)
             , given :: M.Map Identifier (Braced Int Int)
             , wanted :: S.Set Identifier
             }
    deriving Typeable

problem0 :: Problem
problem0 = Problem
    { domain_size_should_be = (EQ, 3)
    , given = M.fromList 
        [(mk 0 "R", Braced (R.make [(1,2), (2,3)])) ]
    , wanted = S.fromList [ mk 0 "S" ]
    , property = prop0
    }

data Solution = 
     Solution { domain_size :: Int
              , assignment :: M.Map Identifier (Braced Int Int)
              }
    deriving Typeable

-- | this is arbitrary: the sum of the numbers
-- of edges of the relations.
instance Size Solution where 
    size s = sum $ map size $ M.elems $ assignment s

solution0 :: Solution
solution0 = Solution
    { domain_size = 3
    , assignment = M.fromList
        [(mk 0 "S", Braced (R.make [(3,2)]))]
    }

data Abstract_Rewriting = Abstract_Rewriting 
    deriving Typeable

derives [makeReader, makeToDoc] 
        [''Problem, ''Solution, ''Abstract_Rewriting ]
derives [makeReader] [''Ordering]

instance Show Abstract_Rewriting where show = render . toDoc

instance OrderScore Abstract_Rewriting where
    scoringOrder _ = Increasing

instance Partial Abstract_Rewriting Problem Solution where
    
    describe _ p = vcat
        [ text "Define relations" 
          <+> toDoc (S.toList $ wanted p)
        , text "on domain [1, 2 .. domain_size] with domain_size"
          <+> let (rel,m) = domain_size_should_be p
              in  text ( case rel of LT -> "<" ; EQ -> "=" ; GT -> ">" ) <+> toDoc m
        , text "such that this property holds:"
          </> toDoc ( property p )
        , text "in this environment:" </> toDoc ( given p )

{-
          </> vcat ( map ( \(k,v) -> 
                      hsep [ toDoc k, equals, toDoc v ]
              ) $ M.toList $ given p )
-}

        ]

    initial _ p = solution0

    partial _ p s = do
        let (cmp, t) = domain_size_should_be p
        when ( not $ cmp == compare (domain_size s) t ) 
            $ reject $ text "invalid domain size"

    total _ p s = do
        let d = S.fromList [ 1 .. domain_size s ]
            rel (Braced r) = R.make_on (d,d) $ R.toList r
            env = M.map rel
                $ M.union -- Note: left-biased, so student
                 -- cannot override what is given
                  (given p) (assignment s)
        (ok, doc) <- prop env ( property p )
        inform doc
        when (not ok) $ reject 
            $ text "The stated property does not hold."

make :: Make
make = direct Abstract_Rewriting problem0
